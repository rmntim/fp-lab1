open Alcotest
module Euler14 = Labwork1.Euler_problems.Euler14
module Euler16 = Labwork1.Euler_problems.Euler16

let test_collatz_next_even () = check int "6 -> 3" 3 (Euler14.collatz_next 6)
let test_collatz_next_odd () = check int "7 -> 22" 22 (Euler14.collatz_next 7)

let test_collatz_length_tail_known () =
  check int "length(13)" 10 (Euler14.collatz_length_tail 13)

let test_collatz_length_consistency () =
  let cases = [ 1; 2; 3; 13; 27; 97 ] in
  List.iter
    (fun n ->
      check int
        (Printf.sprintf "tail vs recursive n=%d" n)
        (Euler14.collatz_length_tail n)
        (Euler14.collatz_length_recursive n))
    cases

let collatz_solvers =
  [
    ("tail", Euler14.solve_monolithic_tail);
    ("recursive", Euler14.solve_monolithic_recursive);
    ("modular", Euler14.solve_modular);
    ("map", Euler14.solve_with_map);
    ("loops", Euler14.solve_with_loops);
    ("seq", Euler14.solve_with_seq);
  ]

let test_collatz_solvers_small_limit () =
  let limit = 10 in
  let expected = 9 in
  List.iter
    (fun (label, solver) -> check int label expected (solver limit))
    collatz_solvers

let test_collatz_tail_large_limit () =
  check int "limit 1_000_000" 837799 (Euler14.solve_monolithic_tail 1_000_000)

let test_power_tail_small () =
  check int "2^15" 26 (Euler16.solve_monolithic_tail 2 15)

let test_power_recursive_small () =
  check int "2^15" 26 (Euler16.solve_monolithic_recursive 2 15)

let test_power_modular_small () =
  check int "2^15" 26 (Euler16.solve_modular 2 15)

let test_power_map_small () = check int "2^15" 26 (Euler16.solve_with_map 2 15)

let test_power_loops_small () =
  check int "2^15" 26 (Euler16.solve_with_loops 2 15)

let test_power_seq_small () = check int "2^15" 26 (Euler16.solve_with_seq 2 15)

let test_power_large_limit () =
  let expected = 1366 in
  List.iter
    (fun (label, solver) -> check int label expected (solver 2 1000))
    [
      ("tail", Euler16.solve_monolithic_tail);
      ("recursive", Euler16.solve_monolithic_recursive);
      ("modular", Euler16.solve_modular);
      ("map", Euler16.solve_with_map);
      ("loops", Euler16.solve_with_loops);
      ("seq", Euler16.solve_with_seq);
    ]

let () =
  run "labwork1"
    [
      ( "Euler14",
        [
          test_case "collatz next even" `Quick test_collatz_next_even;
          test_case "collatz next odd" `Quick test_collatz_next_odd;
          test_case "collatz length tail" `Quick test_collatz_length_tail_known;
          test_case "collatz length consistency" `Quick
            test_collatz_length_consistency;
          test_case "collatz solvers limit 10" `Quick
            test_collatz_solvers_small_limit;
          test_case "collatz tail limit 1_000_000" `Slow
            test_collatz_tail_large_limit;
        ] );
      ( "Euler16",
        [
          test_case "tail 2^15" `Quick test_power_tail_small;
          test_case "recursive 2^15" `Quick test_power_recursive_small;
          test_case "modular 2^15" `Quick test_power_modular_small;
          test_case "map 2^15" `Quick test_power_map_small;
          test_case "loops 2^15" `Quick test_power_loops_small;
          test_case "seq 2^15" `Quick test_power_seq_small;
          test_case "all solvers 2^1000" `Slow test_power_large_limit;
        ] );
    ]
