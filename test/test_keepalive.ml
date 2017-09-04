(* Test the functional part *)

let simulate configuration iterations nprobes ns state =
  let rec loop iterations nprobes ns state =
    if iterations > 3 * configuration.Tcp.Keepalive.probes
    then Alcotest.fail (Printf.sprintf "too many iteractions: loop in keep-alive test? iterations = %d nprobes = %d ns=%Ld" iterations nprobes ns);
    let action, state' = Tcp.Keepalive.next ~configuration ~ns state in
    match action with
    | `SendProbe ->
      Logs.info (fun f -> f "iteration %d, ns %Ld: SendProbe" iterations ns);
      loop (iterations + 1) (nprobes + 1) ns state'
    | `Wait ns' ->
      Logs.info (fun f -> f "iteration %d, ns %Ld: Wait %Ld" iterations ns ns');
      loop (iterations + 1) nprobes (Int64.add ns ns') state'
    | `Close ->
      Logs.info (fun f -> f "iteration %d, ns %Ld: Close" iterations ns);
      nprobes in
  loop iterations nprobes ns state

(* check we send the expected number of probes if everything does as expected *)
let test_keepalive_sequence () =
  let configuration = Tcp.Keepalive.default in
  let state = Tcp.Keepalive.alive in
  let nprobes = simulate configuration 0 0 0L state in
  Alcotest.(check int) "number of probes" (configuration.probes) nprobes

(* check what happens if we miss a probe *)
let test_keepalive_miss_probes () =
  let configuration = Tcp.Keepalive.default in
  let state = Tcp.Keepalive.alive in
  (* skip sending the first 1 or 2 probes *)
  let ns = Int64.(add configuration.Tcp.Keepalive.time (mul 2L configuration.Tcp.Keepalive.interval)) in
  let nprobes = simulate configuration 0 0 ns state in
  if nprobes >= configuration.Tcp.Keepalive.probes
  then Alcotest.fail (Printf.sprintf "too many probes: max was %d but we sent %d and we should have skipped the first 1 or 2" configuration.probes nprobes)

(* check what happens if we exceed the maximum timeout *)
let test_keepalive_miss_everything () =
  let configuration = Tcp.Keepalive.default in
  let state = Tcp.Keepalive.alive in
  (* massive delay *)
  let ns = Int64.(add configuration.Tcp.Keepalive.time (mul 2L (mul (of_int configuration.Tcp.Keepalive.probes) configuration.Tcp.Keepalive.interval))) in  
  let nprobes = simulate configuration 0 0 ns state in
  if nprobes <> 0
  then Alcotest.fail (Printf.sprintf "too many probes: max was %d but we sent %d and we should have skipped all" configuration.probes nprobes)

let suite_1 = [
  "correct number of keepalives", `Quick, test_keepalive_sequence;
  "we don't try to send old keepalives", `Quick, test_keepalive_miss_probes;
  "check we close if we miss all probes", `Quick, test_keepalive_miss_everything;
]

let suite_1 =
  List.map (fun (n, s, f) -> n, s, (fun () -> Lwt.return (f ()))) suite_1

(* Test the end-to-end protocol behaviour *)
open Common
open Vnetif_common

let (>>=) = Lwt.(>>=)

let src = Logs.Src.create "test_keepalive" ~doc:"keepalive tests"
module Log = (val Logs.src_log src : Logs.LOG)

module Test_connect = struct
  module V = VNETIF_STACK (Vnetif_backends.On_off_switch)

  let netmask = 24
  let gw = Some (Ipaddr.V4.of_string_exn "10.0.0.1")
  let client_ip = Ipaddr.V4.of_string_exn "10.0.0.101"
  let server_ip = Ipaddr.V4.of_string_exn "10.0.0.100"
  let test_string = "Hello world from Mirage 123456789...."
  let backend = V.create_backend ()

  let err_read_eof () = failf "accept got EOF while reading"
  let err_write_eof () = failf "client tried to write, got EOF"

  let err_read e =
    let err = Format.asprintf "%a" V.Stackv4.TCPV4.pp_error e in
    failf "Error while reading: %s" err

  let err_write e =
    let err = Format.asprintf "%a" V.Stackv4.TCPV4.pp_write_error e in
    failf "client tried to write, got %s" err

  let accept flow expected =
    let ip, port = V.Stackv4.TCPV4.dst flow in
    Logs.debug (fun f -> f "Accepted connection from %s:%d" (Ipaddr.V4.to_string ip) port);
    V.Stackv4.TCPV4.read flow >>= function
    | Error e      -> err_read e
    | Ok `Eof      -> err_read_eof ()
    | Ok (`Data b) ->
      Lwt_unix.sleep 0.1 >>= fun () ->
      (* sleep first to capture data in pcap *)
      Alcotest.(check string) "accept" expected (Cstruct.to_string b);
      Logs.debug (fun f -> f "Connection closed");
      Lwt.return_unit

  let test_tcp_connect_two_stacks () =
    let timeout = 15.0 in
    Lwt.pick [
      (Lwt_unix.sleep timeout >>= fun () ->
        failf "connect test timedout after %f seconds" timeout) ;

      (V.create_stack backend server_ip netmask gw >>= fun s1 ->
        V.Stackv4.listen_tcpv4 s1 ~port:80 (fun f -> accept f test_string);
        V.Stackv4.listen s1) ;

      (Lwt_unix.sleep 0.1 >>= fun () ->
        V.create_stack backend client_ip netmask gw >>= fun s2 ->
        Lwt.pick [
        V.Stackv4.listen s2;
        (let conn = V.Stackv4.TCPV4.create_connection (V.Stackv4.tcpv4 s2) in
        or_error "connect" conn (server_ip, 80) >>= fun flow ->
        V.Stackv4.TCPV4.enable_keepalive ~t:(V.Stackv4.tcpv4 s2) ~flow ~time:0L ~interval:(Duration.of_sec 1) ~probes:3;
        Logs.debug (fun f -> f "Connected to other end...");
        V.Stackv4.TCPV4.write flow (Cstruct.of_string test_string) >>= function
        | Error `Closed -> err_write_eof ()
        | Error e -> err_write e
        | Ok ()   ->
          Logs.debug (fun f -> f "wrote hello world");
          V.Stackv4.TCPV4.close flow >>= fun () ->
          Lwt_unix.sleep 1.0 >>= fun () -> (* record some traffic after close *)
          Lwt.return_unit)]) ] >>= fun () ->

    Lwt.return_unit

  let record_pcap =
    V.record_pcap backend

end

let test_tcp_connect_two_stacks_basic () =
  Test_connect.record_pcap
    "tcp_connect_two_stacks_basic.pcap"
    Test_connect.test_tcp_connect_two_stacks

let suite_2 = [

  "connect two stacks, basic test", `Quick,
  test_tcp_connect_two_stacks_basic;
]

let suite = suite_1 @ suite_2