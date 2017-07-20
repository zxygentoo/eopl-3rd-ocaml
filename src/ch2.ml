(* e2.1 *)

module Bigit : sig
  (* interfaces *)
  type bigit
  val zero : bigit
  val is_zero : bigit -> bool
  val succ : bigit -> bigit
  val pred : bigit -> bigit

  (* basic arithmetic *)
  val add : bigit -> bigit -> bigit
  val mul : bigit -> bigit -> bigit
  val fact : bigit -> bigit

  (* helpers *)
  val of_int : int -> bigit
  val inspect : bigit -> bigit

end = struct
  type bigit = int list

  (* must >= 1 *)
  let step : int = 16

  let zero = []

  let rec is_zero n =
    n = zero

  let rec succ n =
    match n with
    | [] ->
      [1]
    | [x;] ->
      if x = step - 1 then 0::[1] else [x + 1]
    | x::xs ->
      if x = step - 1 then 0 :: succ xs else x + 1 :: xs

  let rec pred n =
    match n with
    | [] ->
      failwith "Zero has no pred."
    | [x;] ->
      if x = 1 then [] else [x-1]
    | [x;y] -> (
      match x, y with
      | 0, 1    -> [step - 1]
      | 0, y'   -> [step - 1; y'-1]
      | x', y'  -> [x' - 1; y'-1]
    )
    | x::xs ->
      if x = 0 then step - 1 :: pred xs else x - 1 :: xs

  let one = succ zero

  let is_one x =
    is_zero (pred x);;

  let rec add x y =
    match x, y with
    | x, y when is_zero x ->
      y
    | x, y ->
      add (pred x) (succ y)

  let rec mul x y =
    match x, y with
    | x, y when is_zero x || is_zero y ->
      zero
    | x, y when is_one x ->
      y
    | x, y ->
      add y (mul (pred x) y)

  let rec fact n =
    match n with
    | x when is_zero x ->
      one
    | x when is_one x ->
      one
    | x when is_one (pred x) ->
      x
    | x ->
      mul x (fact (pred x))

  let rec of_int i =
    match i with
    | x when x < 0 ->
      failwith "Invalid: negtive ing."
    | 0 ->
      zero
    | x ->
      add one (of_int (i - 1))

  let to_list n =
    n
  
  let inspect n =
    List.iter (Printf.printf "%d;") (to_list n);
    n

end
;;

(* tests *)

assert (Bigit.is_zero Bigit.zero = true);;
assert (Bigit.zero |> Bigit.succ |> Bigit.pred = Bigit.zero);;
Bigit.zero |> Bigit.pred;;
assert (Bigit.add Bigit.zero (Bigit.succ Bigit.zero) = Bigit.succ Bigit.zero);;
assert (
  0
  |> Bigit.of_int
  |> Bigit.succ |> Bigit.pred |> Bigit.succ |> Bigit.succ
  |> Bigit.fact
  |> Bigit.inspect
  =
  (Bigit.zero |> Bigit.succ |> Bigit.succ)
);;
