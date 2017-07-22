(* e2.27 *)

(* e2.28 *)

(* e2.29 *)

(* e2.30 *)

(* e2.31 *)

module Polish : sig
  type token =
    | Num of int
    | Minus
  type exp =
    | Const of int
    | Sub of exp * exp

  val parse_one : token list -> exp * (token list)
  val parse : token list -> exp list

end = struct
  type token =
    | Num of int
    | Minus
  type exp =
    | Const of int
    | Sub of exp * exp

  let rec parse_one lst =
    match lst with
    | [] ->
      failwith "Invalid expressoin."
    | (Num n)::xs ->
      Const n, xs
    | Minus::xs ->
      let e1, xs1 = parse_one xs in
      let e2, xs2 = parse_one xs1 in
      Sub (e1, e2), xs2

  let parse lst =
    let rec parse_aux acc lst =
      match parse_one lst with
      | e, [] ->
        List.rev (e::acc)
      | e, xs ->
        parse_aux (e::acc) xs
    in
      if lst = [] then [] else parse_aux [] lst

end
;;

(* tests *)

open Polish

(* () *)
let e0 = []
(* (42) *)
let e1 = [Num 42];;
(* (42 41) *)
let e2 = [Num 42; Num 41];;
(* (- 42 41) *)
let e3 = [Minus; Num 42; Num 41];;
(* (- 42 41 40) *)
let e4 = [Minus; Minus; Num 42; Num 41; Num 40];;
(* (- - 3 2 - 4 - 12 7) *)
let e5 = [Minus; Minus; Num 3; Num 2; Minus; Num 4; Minus; Num 12; Num 7];;

parse_one e0;;
parse_one e1;;
parse_one e2;;
parse_one e3;;
parse_one e4;;
parse_one e5;;

parse e0;;
parse e1;;
parse e2;;
parse e3;;
parse e4;;
parse e5;;
