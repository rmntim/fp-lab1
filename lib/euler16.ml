let explode_string s = List.init (String.length s) (String.get s)
let int_from_char c = int_of_char c - int_of_char '0'

let solve_monolithic_tail base power =
  let rec aux acc = function
    | [] -> acc
    | d :: rest -> aux (acc + int_from_char d) rest
  in
  Z.pow (Z.of_int base) power |> Z.to_string |> explode_string |> aux 0

let solve_monolithic_recursive base power =
  let rec aux = function [] -> 0 | d :: rest -> int_from_char d + aux rest in
  Z.pow (Z.of_int base) power |> Z.to_string |> explode_string |> aux

module PowerGenerator = struct
  let generate base power =
    Z.pow (Z.of_int base) power |> Z.to_string |> explode_string
end

module PowerFilter = struct
  let filter = List.filter (( <> ) '0')
end

module PowerReducer = struct
  let reduce = List.fold_left (fun acc c -> acc + int_from_char c) 0
end

let solve_modular base power =
  PowerGenerator.generate base power
  |> PowerFilter.filter |> PowerReducer.reduce

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
