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
    | Sub of (exp * exp)
    | IsZero of exp
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
    | Sub of (exp * exp)
    | IsZero of exp
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
    match expr with
    | Num i ->
      Int i

    | Sub (exp1, exp2) ->
      let exp_to_int exp = val_to_num (value_of exp env) in
      Int (exp_to_int exp1 - exp_to_int exp2)

    | IsZero exp ->
      Bool (val_to_num (value_of exp env) = 0)

    | IF (pred_exp, then_exp, else_exp) ->
      if val_to_bool (value_of pred_exp env)
      then value_of then_exp env
      else value_of else_exp env

    | Ident id ->
      Env.apply_env id env

    | LetExp (id, exp_var, exp_body) ->
      let new_env = Env.extend_env id (value_of exp_var env) env in
      value_of exp_body new_env
end
;;


(* tests *)

open LetLang;;

let exp1 = Sub (Num 55, Sub (Num 22, Num 11));;
let exp2 = Sub (Num 55, Sub (Ident "x", Num 11));;
let exp3 = IF ((IsZero (Num 0)), exp1, exp2);;
let exp4 = IF ((IsZero (Num 1)), exp1, exp2);;

let env1 = Env.empty_env;;
let env2 = (Env.extend_env "x" (Int 99) Env.empty_env);;

assert (value_of exp1 env1 = Int 44);;
assert (value_of exp2 env2 = Int (-33));;
assert (value_of exp3 env2 = Int 44);;
assert (value_of exp4 env2 = Int (-33));;
