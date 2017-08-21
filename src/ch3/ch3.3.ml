(* ch3.3 ProcLang: a letlang with function *)

module FunLang : sig

  (* env *)
  module Env : sig
    type 'a t = (string * 'a) list
    val empty_env : 't t
    val extend_env : string -> 'a -> 'a t -> 'a t
    val apply_env : string -> 'a t -> 'a
  end

  type exp

  type value

  val eval : exp -> value Env.t -> value

  (* test *)
  module Test : sig
    val run_test : unit -> unit
  end

end = struct

  (* env *)
  module Env = struct
    type 'a t = (string * 'a) list

    let empty_env =
      []

    let is_empty_env env =
      env = []

    let extend_env id value env =
      (id, value) :: env

    let rec search_var id env =
      match env with
      | [] -> None
      | (var', val') :: xs when var' = id -> Some val'
      | x::xs -> search_var id xs

    let apply_env id env =
      match search_var id env with
        | None -> failwith "empty env or has no binding for variable."
        | Some value -> value

  end

  (* funlang *)

  type id = string

  type exp =
    | IntLit of int
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

  let fn_of_val =
    function
    | Fn (var, body, env) -> var, body, env
    | _ -> failwith "must be of type Fn."

  let rec eval exp env =

    let int_binary_op e1 e2 env op =
      let ev e = eval e env in
      Int (op (int_of_val (ev e1)) (int_of_val (ev e2)))
    in

    let apply fn arg =
      let var, body, saved_env = fn_of_val fn in
      let new_env = Env.extend_env var arg saved_env in
      eval body new_env
    in

    match exp with
    | IntLit n -> Int n

    | Id i -> Env.apply_env i env

    | IsZero x ->
      if (int_of_val (eval x env)) = 0
      then Bool true
      else Bool false

    | Add (e1, e2) -> int_binary_op e1 e2 env ( + )

    | Sub (e1, e2) -> int_binary_op e1 e2 env ( - )

    | Mul (e1, e2) -> int_binary_op e1 e2 env ( * )

    | Div (e1, e2) -> int_binary_op e1 e2 env ( / )

    | If (pred, then_exp, else_exp) ->
      if (int_of_val (eval pred env)) = 0
      then (eval then_exp env)
      else (eval else_exp env)

    | Let (var, bind, body) ->
      let var_value = eval bind env in
      let new_env = Env.extend_env var var_value env in
      eval body new_env

    | Fun (var, body) ->
      Fn (var, body, env)

    | App (rator, rand) ->
      apply (eval rator env) (eval rand env)

  (* test *)
  module Test = struct

    (* let f = proc (x) -(x,11) in (f (f 77)) ==> Int 55 *)
    let exp1 = Let (
        "f",
        Fun ("x", Sub (Id "x", IntLit 11)),
        App (Id "f", App (Id "f", IntLit 77))
      )

    (* (proc (f) (f (f 77)) proc (x) -(x, 11)) ==> Int 55 *)
    let exp2 = Let (
        "f",
        Fun ("f", App (Id "f", App (Id "f", IntLit 77))),
        Let (
          "g",
          Fun (("x", Sub (Id "x", IntLit 11))),
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

end
;;

(* tests *)

FunLang.Test.run_test () ;;
