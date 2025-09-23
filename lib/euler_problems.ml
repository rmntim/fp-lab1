module Euler14 = struct
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

  let solve_modular limit =
    let generate bound = List.init (bound + 1) Fun.id in
    let filter_numbers bound = List.filter (fun n -> n > 0 && n < bound) in
    let reduce_numbers =
      List.fold_left (fun (best, best_len) n ->
          let len = collatz_length_tail n in
          if len > best_len then (n, len) else (best, best_len))
    in
    generate limit |> filter_numbers limit |> reduce_numbers (1, 1) |> fst

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
end

module Euler16 = struct
  let explode_string s = List.init (String.length s) (String.get s)
  let int_from_char c = int_of_char c - int_of_char '0'

  let solve_monolithic_tail base power =
    let rec aux acc = function
      | [] -> acc
      | d :: rest -> aux (acc + int_from_char d) rest
    in
    Z.pow (Z.of_int base) power |> Z.to_string |> explode_string |> aux 0

  let solve_monolithic_recursive base power =
    let rec aux = function
      | [] -> 0
      | d :: rest -> int_from_char d + aux rest
    in
    Z.pow (Z.of_int base) power |> Z.to_string |> explode_string |> aux

  let solve_modular base power =
    let generate base power =
      Z.pow (Z.of_int base) power |> Z.to_string |> explode_string
    in
    let filter = List.filter (( <> ) '0') in
    let reduce = List.fold_left (fun acc c -> acc + int_from_char c) 0 in
    generate base power |> filter |> reduce

  let solve_with_map base power =
    Z.pow (Z.of_int base) power
    |> Z.to_string |> explode_string |> List.map int_from_char
    |> List.fold_left ( + ) 0

  let solve_with_loops base power =
    let acc = ref 0 in
    let numbers = Z.pow (Z.of_int base) power |> Z.to_string in
    for n = 0 to String.length numbers - 1 do
      let number = int_from_char numbers.[n] in
      acc := !acc + number
    done;
    !acc

  let solve_with_seq base power =
    Z.pow (Z.of_int base) power
    |> Z.to_string |> String.to_seq |> Seq.map int_from_char
    |> Seq.fold_left ( + ) 0
end
