
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
  let ns = Duration.of_sec (configuration.Tcp.Keepalive.time + 2 * configuration.Tcp.Keepalive.interval) in
  let nprobes = simulate configuration 0 0 ns state in
  if nprobes >= configuration.Tcp.Keepalive.probes
  then Alcotest.fail (Printf.sprintf "too many probes: max was %d but we sent %d and we should have skipped the first 1 or 2" configuration.probes nprobes)

(* check what happens if we exceed the maximum timeout *)
let test_keepalive_miss_everything () =
  let configuration = Tcp.Keepalive.default in
  let state = Tcp.Keepalive.alive in
  (* massive delay *)
  let ns = Duration.of_sec (configuration.Tcp.Keepalive.time + 2 * configuration.Tcp.Keepalive.probes * configuration.Tcp.Keepalive.interval) in
  let nprobes = simulate configuration 0 0 ns state in
  if nprobes <> 0
  then Alcotest.fail (Printf.sprintf "too many probes: max was %d but we sent %d and we should have skipped all" configuration.probes nprobes)

let suite = [
  "correct number of keepalives", `Quick, test_keepalive_sequence;
  "we don't try to send old keepalives", `Quick, test_keepalive_miss_probes;
  "check we close if we miss all probes", `Quick, test_keepalive_miss_everything;
]

let suite =
  List.map (fun (n, s, f) -> n, s, (fun () -> Lwt.return (f ()))) suite
