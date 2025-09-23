let collatz_next n = if n land 1 = 0 then n / 2 else (3 * n) + 1

let collatz_length_tail n =
  let rec aux acc current =
    if current = 1 then acc + 1 else aux (acc + 1) (collatz_next current)
  in
  aux 0 n

let rec collatz_length_recursive n =
  if n = 1 then 1 else 1 + collatz_length_recursive (collatz_next n)

let solve_monolithic_tail limit =
  let rec search n best best_len =
    if n = limit then best
    else
      let len = collatz_length_tail n in
      if len > best_len then search (n + 1) n len
      else search (n + 1) best best_len
  in
  search 1 1 1

let solve_monolithic_recursive limit =
  let rec search n best best_len =
    if n = limit then best
    else
      let len = collatz_length_recursive n in
      if len > best_len then search (n + 1) n len
      else search (n + 1) best best_len
  in
  search 1 1 1

module CollatzGenerator = struct
  let generate bound = List.init (bound + 1) Fun.id
end

module CollatzFilter = struct
  let filter_numbers bound = List.filter (fun n -> n > 0 && n < bound)
end

module CollatzReducer = struct
  let reduce_numbers =
    List.fold_left (fun (best, best_len) n ->
        let len = collatz_length_tail n in
        if len > best_len then (n, len) else (best, best_len))
end

let solve_modular limit =
  CollatzGenerator.generate limit
  |> CollatzFilter.filter_numbers limit
  |> CollatzReducer.reduce_numbers (1, 1)
  |> fst

let solve_with_map limit =
  List.init (limit - 1) (fun i -> i + 1)
  |> List.map (fun n -> (n, collatz_length_tail n))
  |> List.fold_left
       (fun (best, best_len) (n, len) ->
         if len > best_len then (n, len) else (best, best_len))
       (1, 1)
  |> fst

let solve_with_loops limit =
  let best = ref 1 in
  let best_len = ref 1 in
  for n = 1 to limit - 1 do
    let len = collatz_length_tail n in
    if len > !best_len then (
      best := n;
      best_len := len)
  done;
  !best

let solve_with_seq limit =
  let open Seq in
  let naturals = unfold (fun state -> Some (state, state + 1)) 1 in
  let step (best, best_len) n =
    let len = collatz_length_tail n in
    if len > best_len then (n, len) else (best, best_len)
  in
  naturals |> take_while (fun n -> n < limit) |> fold_left step (1, 1) |> fst
