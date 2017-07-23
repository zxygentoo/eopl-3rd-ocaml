(* syntax for the LET language *)

type exp =
  | Num of int
  | Sub of (exp * exp)
  | IsZero of exp
  | IF of (exp * exp * exp)
  | Ident of string
  | LetExp of (string * exp * exp)
;;

type program = Prog of exp
;;

(* -(55, -(x, 11)) *)
Prog (Sub (Num 55, Sub (Ident "x", Num 11)));;
