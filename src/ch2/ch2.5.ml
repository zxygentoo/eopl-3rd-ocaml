(* e2.27 *)

(* e2.28 *)

(* e2.29 *)

(* e2.30 *)

(* e2.31 *)

module Polish : sig
  type expr =
    | Num of int
    | Minus
  type prefix_list = expr list
  type prefix_exp =
    | Const of int
    | Diff of prefix_exp * prefix_exp

  val parse_one : prefix_list -> prefix_exp option * prefix_list  
  val parse : prefix_list -> prefix_exp

end = struct
  type expr =
    | Num of int
    | Minus
  type prefix_list = expr list
  type prefix_exp =
    | Const of int
    | Diff of prefix_exp * prefix_exp

  let rec parse_one lst =
    let parse_one_more cont_lst =
      match parse_one cont_lst with
      | None, _ ->
        failwith "Invalid expression."
      | Some e, rem ->
        e, rem

    in match lst with
      | [] ->
        None, []
      | Num n :: xs ->
        Some (Const n), xs
      | Minus :: xs ->
        let e', xs' = parse_one_more xs; in
        let e'', xs'' = parse_one_more xs' in
        Some (Diff (e', e'')), xs''

  let rec parse lst =
    match parse_one lst with
    | None, _ ->
      failwith "Invalid expression."
    | Some e, [] ->
      e
    | Some e, x::xs ->
      failwith "Invaid expression."

end
;;

(* tests *)

(* () *)
let e0 = []
(* (42) *)
let e1 = [Polish.Num 42];;
(* (42 41) *)
let e2 = [Polish.Num 42; Polish.Num 41];;
(* (- 42 41) *)
let e3 = [Polish.Minus; Polish.Num 42; Polish.Num 41];;
(* (- 42 41 40) *)
let e4 = [
  Polish.Minus; Polish.Minus; Polish.Num 42; Polish.Num 41; Polish.Num 40
];;
(* (- - 3 2 - 4 - 12 7) *)
let e5 = [
  Polish.Minus; Polish.Minus; Polish.Num 3; Polish.Num 2; Polish.Minus;
  Polish.Num 4; Polish.Minus; Polish.Num 12; Polish.Num 7
];;

Polish.parse_one e0;;
Polish.parse_one e1;;
Polish.parse_one e2;;
Polish.parse_one e3;;
Polish.parse_one e4;;
Polish.parse_one e5;;

Polish.parse e0;;
Polish.parse e1;;
Polish.parse e2;;
Polish.parse e3;;
Polish.parse e4;;
Polish.parse e5;;
