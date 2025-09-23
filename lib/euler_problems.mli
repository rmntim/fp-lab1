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
  val solve_monolithic_tail : int -> int -> int
  val solve_monolithic_recursive : int -> int -> int
  val solve_modular : int -> int -> int
  val solve_with_map : int -> int -> int
  val solve_with_loops : int -> int -> int
  val solve_with_seq : int -> int -> int
end
