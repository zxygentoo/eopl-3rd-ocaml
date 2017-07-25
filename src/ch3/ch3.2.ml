(* e3.6 3.7 3.8 *)

module Env : sig
  type 't env = (string * 't) list
  val empty_env : 't env
  val extend_env : string -> 't -> 't env -> 't env
  val apply_env : string -> 't env -> 't

end = struct
  type 't env = (string * 't) list

  let empty_env =
    []

  let is_empty_env env =
    env = []

  let extend_env s v env =
    (s, v)::env

  let rec search_var s env=
    match env with
    | [] ->
      None
    | (var', val')::xs when var' = s ->
      Some val'
    | x::xs ->
      search_var s xs

  let apply_env s env =
    if is_empty_env env then failwith "Empty enviroment."
    else match search_var s env with
    | None ->
      failwith "Enviroment has no binding for variable."      
    | Some v ->
      v
end
;;

module LetLang : sig
  (* Syntax of LET language *)
  type exp =
    | Num of int
    | Add of (exp * exp)
    | Sub of (exp * exp)
    | Mul of (exp * exp)
    | Div of (exp * exp)
    | IsZero of exp
    | EQ of (exp * exp)
    | GT of (exp * exp)
    | LT of (exp * exp)
    | IF of (exp * exp * exp)
    | Ident of string
    | LetExp of (string * exp * exp)
  
  type program = Prog of exp
  
  (* all possible values in LET language *)
  type value =
    | Int of int
    | Bool of bool
  
  (* expressed value *)
  type exp_val = value
  
  (* denoted value *)
  type den_val = value
  
  (* interface *)
  val value_of : exp -> value Env.env -> den_val

end = struct
  (* Syntax of LET language *)
  type exp =
    | Num of int
    | Add of (exp * exp)
    | Sub of (exp * exp)
    | Mul of (exp * exp)
    | Div of (exp * exp)
    | IsZero of exp
    | EQ of (exp * exp)
    | GT of (exp * exp)
    | LT of (exp * exp)
    | IF of (exp * exp * exp)
    | Ident of string
    | LetExp of (string * exp * exp)
  type program = Prog of exp
  type value =
    | Int of int
    | Bool of bool
  type exp_val = value
  type den_val = value

  exception Invalid

  let num_val i =
    Int i

  let bool_val x =
    Bool x

  let val_to_num =
    function
    | Int n -> n
    | _ -> raise Invalid

  let val_to_bool =
    function
    | Bool x -> x
    | _ -> raise Invalid

  let rec value_of expr env =
    let exp_to_int expr =
      val_to_num (value_of expr env)
    in

    let int_compare expr1 expr2 f =
      let v1 = value_of expr1 env in
      let v2 = value_of expr2 env
      in match v1, v2 with
      | Int x, Int y -> Bool (f x y)
      | _ -> raise Invalid
    in
    
    match expr with
    | Num i ->
      Int i

    | Add (exp1, exp2) ->
      Int (exp_to_int exp1 + exp_to_int exp2)

    | Sub (exp1, exp2) ->
      Int (exp_to_int exp1 - exp_to_int exp2)

    | Mul (exp1, exp2) ->
      Int (exp_to_int exp1 * exp_to_int exp2)

    | Div (exp1, exp2) ->
      Int (exp_to_int exp1 / exp_to_int exp2)

    | IsZero exp ->
      Bool (val_to_num (value_of exp env) = 0)

    | EQ (exp1, exp2) ->
      int_compare exp1 exp2 (=)
    
    | LT (exp1, exp2) ->
      int_compare exp1 exp2 (<)

    | GT (exp1, exp2) ->
      int_compare exp1 exp2 (>)

    | IF (pred_exp, then_exp, else_exp) ->
      if val_to_bool (value_of pred_exp env)
      then value_of then_exp env
      else value_of else_exp env

    | Ident id ->
      Env.apply_env id env

    | LetExp (var_name, var_exp, body_exp) ->
      let new_env =
        Env.extend_env var_name (value_of var_exp env) env
      in
      value_of body_exp new_env

end
;;


(* tests *)

open LetLang;;


let n0 = Num 0;;
let n1 = Num 11;;
let n2 = Num 22;;

let env0 = Env.empty_env;;

assert (value_of (IsZero n0) env0 = Bool true);;
assert (value_of (IsZero n1) env0 = Bool false);;


let sub1 = Sub (n0, n1);;
let sub2 = Sub (n2, sub1);;
let if1 = IF ((IsZero (Num 0)), sub1, sub2);;
let if2 = IF ((IsZero (Num 1)), sub1, sub2);;

assert (value_of sub1 env0 = Int (-11));;
assert (value_of sub2 env0 = Int 33);;
assert (value_of if1 env0 = Int (-11));;
assert (value_of if2 env0 = Int 33);;

let env1 = Env.empty_env;;
let env2 = (Env.extend_env "x" (Int 99) Env.empty_env);;

value_of (GT ((Num 1), (Num 2))) env0;;
value_of (GT ((Num 2), (Num 1))) env0;;

(* 
[env : a -> 3]
let x = 2 * a in x + 1
*)

value_of (
  LetExp (
    "x",
    Mul (
      Num (2),
      Ident ("a")
    ),
    Add (
      Ident ("x"),
      Num (1)
    )
  )
)
(Env.extend_env "a" (Int 3) Env.empty_env)
;;
