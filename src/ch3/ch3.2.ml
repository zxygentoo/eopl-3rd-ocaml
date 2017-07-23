(* Env *)
module Env : sig
  type 't env = (string * 't) list
  val empty_env : 't env
  val extend_env : string -> 't -> 't env -> 't env
  val apply_env : string -> 't env -> 't
  (* val is_empty_env : 't env -> bool *)
  (* val has_binding : string -> 't env -> bool *)
  (* val extend_env_multiple : (string * 't) list -> 't env -> 't env *)
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

  (*  
  let has_binding s env =
    match search_var s env with
    | None ->
      false
    | Some _ ->
      true

  let rec extend_env_multiple alist env =
    match alist with
    | [] ->
      env
    | (s, v)::xs ->
      extend_env_multiple xs (extend_env s v env)
 *)
end
;;


(* Syntax of LET language *)
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

(* all possible values in LET language *)
type value =
  | Int of int
  | Bool of bool
;;

(* expressed value *)
type exp_val = value
;;

(* denoted value *)
type den_val = value
;;

exception Invalid
;;

(* interface *)

let num_val i = Int i
;;

let bool_val x = Bool x
;;

let val_to_num =
  function
  | Int n -> n
  | _ -> raise Invalid
;;

let val_to_bool =
  function
  | Bool x -> x
  | _ -> raise Invalid
;;

let rec value_of expr env =
  match expr with
  | Num i ->
    Int i

  | Sub (exp1, exp2) ->
    Int (val_to_num (value_of exp1 env) - val_to_num (value_of exp2 env))

  | IsZero exp ->
    Bool (val_to_num (value_of exp env) = 0)

  | IF (pred_exp, then_exp, else_exp) ->
    if val_to_bool (value_of pred_exp env)
    then value_of then_exp env
    else value_of else_exp env

  | Ident id ->
    Env.apply_env id env

  | LetExp (id, exp1, exp2) ->
    value_of exp2 (Env.extend_env id (value_of exp1 env) env)
;;

(* tests *)

let exp1 = Sub (Num 55, Sub (Num 22, Num 11));;
let exp2 = Sub (Num 55, Sub (Ident "x", Num 11));;
let exp3 = IF ((IsZero (Num 0)), exp1, exp2);;

value_of exp1 Env.empty_env;;
value_of exp2 (Env.extend_env "x" (Int 99) Env.empty_env);;
value_of exp3 (Env.extend_env "x" (Int 99) Env.empty_env);;
