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
    Array.init (limit - 1) (fun i -> i + 1)
    |> Array.map (fun n -> (n, collatz_length_tail n))
    |> Array.fold_left
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
  type digits = int list

  let normalize digits =
    let rec aux carry acc = function
      | [] when carry = 0 -> acc
      | [] -> carry :: acc
      | d :: rest ->
          let total = d + carry in
          aux (total / 10) ((total mod 10) :: acc) rest
    in
    aux 0 [] digits |> List.rev

  let scale_digits factor digits =
    digits |> List.map (fun d -> d * factor) |> normalize

  let rec scale_digits_recursive factor = function
    | [] -> []
    | d :: rest ->
        let scaled_tail = scale_digits_recursive factor rest in
        normalize ((d * factor) :: scaled_tail)

  let sum_digits digits = List.fold_left ( + ) 0 digits

  let rec power_tail factor power digits =
    if power = 0 then digits
    else power_tail factor (power - 1) (scale_digits factor digits)

  let rec power_recursive factor power digits =
    if power = 0 then digits
    else
      power_recursive factor (power - 1) (scale_digits_recursive factor digits)

  let solve_monolithic_tail base power =
    power_tail base power [ 1 ] |> sum_digits

  let solve_monolithic_recursive base power =
    power_recursive base power [ 1 ] |> sum_digits

  let solve_modular base power =
    let generate_steps count = List.init count Fun.id in
    let filter_steps = List.filter (fun _ -> true) in
    let fold_steps =
      List.fold_left (fun digits _ -> scale_digits base digits) [ 1 ]
    in
    generate_steps power |> filter_steps |> fold_steps |> sum_digits

  let solve_with_map base power =
    List.init power (fun _ -> base)
    |> List.map Fun.id
    |> List.fold_left (fun digits factor -> scale_digits factor digits) [ 1 ]
    |> sum_digits

  let solve_with_loops base power =
    let digits = ref [ 1 ] in
    for _ = 1 to power do
      digits := scale_digits base !digits
    done;
    sum_digits !digits

  let solve_with_seq base power =
    let open Seq in
    let powers = iterate (scale_digits base) [ 1 ] in
    match uncons (drop power powers) with
    | None -> 0
    | Some (digits, _) -> sum_digits digits
end
