(* e2.1 *)

module type Bigit =
sig
  type bigit
  val zero : bigit
  val is_zero : bigit -> bool
  val successor : bigit -> bigit
  val predecessor : bigit -> bigit
end

module Bigit =
struct
  type bigit = int list

  exception Exn of string

  (* step >= 1 *)
  let step : int = 16

  let zero = []

  let rec is_zero n =
    n = zero

  let rec successor n =
    match n with
    | [] ->
      [1]
    | [a] when a = step - 1 ->
      [0; 1]
    | [a] ->
      [a + 1]
    | x::xs when x = step - 1 ->
      0::successor(xs)
    | x::xs ->
      (x + 1)::xs

  let rec predecessor n =
    match n with
    | [] ->
      raise (Exn "Zero has no predecessor.")
    | [1] ->
      []
    | [a] ->
      [a - 1]
    | [0; 1] ->
      [step - 1]
    | [0; b] ->
      [step - 1; b - 1]
    | [a; b] ->
      [a - 1; b]
    | x::xs when x = 0 ->
      (step - 1)::predecessor(xs)
    | x::xs ->
      (x - 1)::xs

end
;;

(* tests *)
assert (Bigit.is_zero Bigit.zero = true);;
assert (Bigit.zero |> Bigit.successor |> Bigit.predecessor = Bigit.zero);;
(* error *)
Bigit.zero |> Bigit.predecessor;;


let zero = Bigit.zero;;
let is_zero = Bigit.is_zero;;
let pred = Bigit.predecessor;;
let succ = Bigit.successor;;
let one = succ zero;;
let is_one x = Bigit.is_zero (Bigit.predecessor x);;

let rec add x y =
  match x, y with
  | x, y when is_zero x ->
    y
  | x, y ->
    add (pred x) (succ y)
;;

let rec mul x y =
  match x, y with
  | x, y when is_zero x || is_zero y ->
    zero
  | x, y when is_one x ->
    y
  | x, y ->
    add y (mul (pred x) y)
;;

let rec factorial n =
  match n with
  | x when is_zero x ->
    one
  | x when is_zero (pred x) ->
    one
  | x when is_zero (pred (pred x)) ->
    x
  | x ->
    mul x (factorial (pred x))
;;
