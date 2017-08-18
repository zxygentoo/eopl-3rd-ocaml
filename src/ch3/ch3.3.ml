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
    | Boolean of bool
    | Id of id
    | IsZero of exp
    | Add of exp * exp
    | Sub of exp * exp
    | Mul of exp * exp
    | Div of exp * exp
    | If of exp * exp * exp
    | Let of id * exp * exp
    | Fun of id * exp
    | App of exp * exp

  type value =
    | Int of int
    | Bool of bool
    | Fn of id * exp * value Env.t

  val eval : exp -> value Env.t -> value

end = struct
  type id = string

  type exp =
    | Num of int
    | Boolean of bool
    | Id of id
    | IsZero of exp
    | Add of exp * exp
    | Sub of exp * exp
    | Mul of exp * exp
    | Div of exp * exp
    | If of exp * exp * exp
    | Let of id * exp * exp
    | Fun of id * exp
    | App of exp * exp

  type value =
    | Int of int
    | Bool of bool
    | Fn of id * exp * value Env.t

  let int_of_val =
    function
    | Int i -> i
    | _ -> failwith "must be of type Int."    

  let bool_of_val =
    function
    | Bool x -> x
    | _ -> failwith "must be of type Bool."

  let fn_of_val =
    function
    | Fn (var, body, env) -> var, body, env
    | _ -> failwith "must be of type Fn."

  let rec eval exp env =

    let int_binary_op e1 e2 env op =
      let ev e = eval e env in
      Int (op (int_of_val (ev e1)) (int_of_val (ev e2)))
    in

    let apply_fn rator rand env =
      let fn = eval rator env in
      let arg = eval rand env in
      let apply fn arg =
        let var, body, saved_env = fn_of_val fn in
        let new_env = Env.extend_env var arg saved_env in
        eval body new_env
      in
      apply fn arg
    in

    match exp with
    | Num n ->
      Int n

    | Boolean x ->
      Bool x

    | Id i ->
      Env.apply_env i env

    | IsZero x ->
      let i = int_of_val (eval x env) in
      if i = 0 then Bool true else Bool false

    | Add (e1, e2) ->
      int_binary_op e1 e2 env ( + )

    | Sub (e1, e2) ->
      int_binary_op e1 e2 env ( - )

    | Mul (e1, e2) ->
      int_binary_op e1 e2 env ( * )

    | Div (e1, e2) ->
      int_binary_op e1 e2 env ( / )

    | If (pred, then_exp, else_exp) ->
      if (bool_of_val (eval pred env))
      then (eval then_exp env)
      else (eval else_exp env)

    | Let (var, bind, body) ->
      let v = eval bind env in
      let new_env = Env.extend_env var v env in
      eval body new_env

    | Fun (var, body) ->
      Fn (var, body, env)

    | App (rator, rand) ->
      apply_fn rator rand env

end
;;


(* tests *)
module FunLangTest = struct

  open FunLang

  (* let f = proc (x) -(x,11) in (f (f 77)) ==> Int 55 *)
  let exp1 = Let (
      "f",
      Fun ("x", Sub (Id "x", Num 11)),
      App (Id "f", App (Id "f", Num 77))
    )

  (* (proc (f) (f (f 77)) proc (x) -(x, 11)) ==> Int 55 *)
  let exp2 = Let (
      "f",
      Fun ("f", App (Id "f", App (Id "f", Num 77))),
      Let (
        "g",
        Fun (("x", Sub (Id "x", Num 11))),
        App (Id "f", Id "g")
      )
    )

  let env0 = Env.empty_env

  let run_test () =
    Printf.printf "runing FunLang test...\n" ;
    begin
      (assert (eval exp1 env0 = Int 55)) ;
      (assert (eval exp2 env0 = Int 55))    
    end

end
;;

FunLangTest.run_test () ;;
