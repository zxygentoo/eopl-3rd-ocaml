(* ch3.3 ProcLang: a letlang with function *)

(* env *)
module Env : sig
  type 'a t = (string * 'a) list
  val empty_env : 't t
  val extend_env : string -> 'a -> 'a t -> 'a t
  val apply_env : string -> 'a t -> 'a

end = struct
  type 'a t = (string * 'a) list

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


(* FunLang *)
module FunLang : sig
  type id = string

  type exp =
    | Num of int
    | Id of id
    | Sub of exp * exp
    | Let of id * exp * exp
    | Fun of id * exp
    | App of exp * exp

  type value =
    | Int of int
    | Fn of id * exp * value Env.t
  
  val eval : exp -> value Env.t -> value

end = struct
  type id = string

  type exp =
    | Num of int
    | Id of id
    | Sub of exp * exp
    | Let of id * exp * exp
    | Fun of id * exp
    | App of exp * exp

  type value =
    | Int of int
    | Fn of id * exp * value Env.t

  let int_of_val =
    function
    | Int i -> i
    | _ -> failwith "must be of type Int."    

  let fn_of_val =
    function
    | Fn (var, body, env) -> var, body, env
    | _ -> failwith "must be of type Fn"

  let rec eval exp env =
    match exp with
    | Num n ->
      Int n

    | Id i ->
      Env.apply_env i env
    
    | Sub (e1, e2) ->
      let ev e = int_of_val (eval e env) in
      Int (ev e1 - ev e2)

    | Let (var, bind, body) ->
      let v = eval bind env in
      let new_env = Env.extend_env var v env in
      eval body new_env
    
    | Fun (var, body) ->
      Fn (var, body, env)
    
    | App (rator, rand) ->
      let fn = eval rator env in
      let arg = eval rand env in
      let apply fn arg =
        let var, body, old_env = fn_of_val fn in
        let new_env = Env.extend_env var arg old_env in
        eval body new_env
      in
      apply fn arg

end
;;


(* tests *)
open FunLang ;;

(*
  let f = proc (x) -(x,11)
  in (f (f 77)) 
  ==> Int 55
*)
let e = Let (
  "f",
  Fun ("x", Sub (Id "x", Num 11)),
  App (Id "f", App (Id "f", Num 77))
)
in 
  (assert (eval e Env.empty_env = Int 55))
;;

(* 
  (
    proc (f) (f (f 77))
    proc (x) -(x, 11)
  )
  ==> Int 55
*)
let f = Fn ("f", App (Id "f", App (Id "f", Num 77)), Env.empty_env) in
let g = Fn ("x", Sub (Id "x", Num 11), Env.empty_env) in
let e = App (Id "f", Id "g") in
let env = 
  Env.empty_env
  |> Env.extend_env "f" f
  |> Env.extend_env "g" g
in
  (assert (eval e env = Int 55))
;;
