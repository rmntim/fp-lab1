module Euler14 : sig
  val collatz_next : int -> int
  val collatz_length_tail : int -> int
  val collatz_length_recursive : int -> int
  val solve_monolithic_tail : int -> int
  val solve_monolithic_recursive : int -> int
  val solve_modular : int -> int
  val solve_with_map : int -> int
  val solve_with_loops : int -> int
  val solve_with_seq : int -> int
end

module Euler16 : sig
  type digits = int list

  val normalize : int list -> int list
  val scale_digits : int -> int list -> int list
  val scale_digits_recursive : int -> int list -> int list
  val sum_digits : int list -> int
  val power_tail : int -> int -> int list -> int list
  val power_recursive : int -> int -> int list -> int list
  val solve_monolithic_tail : int -> int -> int
  val solve_monolithic_recursive : int -> int -> int
  val solve_modular : int -> int -> int
  val solve_with_map : int -> int -> int
  val solve_with_loops : int -> int -> int
  val solve_with_seq : int -> int -> int
end
