open Labwork1

let print_results title results =
  Printf.printf "%s\n" title;
  List.iter
    (fun (label, value) -> Printf.printf "  %-32s %d\n" label value)
    results;
  print_endline ""

let () =
  let limit_14 = 1_000_000 in
  let base = 2 in
  let power = 1_000 in
  let module P14 = Euler14 in
  let module P16 = Euler16 in
  let problem14_results =
    [
      ("Tail recursion", P14.solve_monolithic_tail limit_14);
      ("Plain recursion", P14.solve_monolithic_recursive limit_14);
      ("Modular pipeline", P14.solve_modular limit_14);
      ("Map-based", P14.solve_with_map limit_14);
      ("Loops", P14.solve_with_loops limit_14);
      ("Seq infinite", P14.solve_with_seq limit_14);
    ]
  in
  let problem16_results =
    [
      ("Tail recursion", P16.solve_monolithic_tail base power);
      ("Plain recursion", P16.solve_monolithic_recursive base power);
      ("Modular pipeline", P16.solve_modular base power);
      ("Map-based", P16.solve_with_map base power);
      ("Loops", P16.solve_with_loops base power);
      ("Seq infinite", P16.solve_with_seq base power);
    ]
  in
  print_results "Problem 14" problem14_results;
  print_results "Problem 16" problem16_results
