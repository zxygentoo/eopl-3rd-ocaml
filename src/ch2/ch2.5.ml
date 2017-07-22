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
    | Diff of exp * exp

  val parse_one : token list -> exp * (token list)
  val parse : token list -> exp list

end = struct
  type token =
    | Num of int
    | Minus
  type exp =
    | Const of int
    | Diff of exp * exp

  let rec parse_one lst =
    match lst with
    | [] ->
      failwith "Bad parse."
    | Num n :: xs ->
      Const n, xs
    | Minus :: xs ->
      let e1, xs1 = parse_one xs in
      let e2, xs2 = parse_one xs1 in
      Diff (e1, e2), xs2

  let parse lst =
    let rec parse_aux acc lst =
      match parse_one lst with
      | e, [] ->
        List.rev (e::acc)
      | e, xs ->
        parse_aux (e::acc) xs
    in
      parse_aux [] lst

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
