(* ch3.4 LetRecLang: a letlang with function *)

module LetRecLang : sig

  (* env *)
  module Env : sig
    type 'a t = (string * 'a) list
    val empty_env : 't t
    val extend_env : string -> 'a -> 'a t -> 'a t
    val extend_env_rec : string -> string -> 'a -> 'a t -> 'a t
    val apply_env : string -> 'a t -> 'a
  end

  type exp
  
  type value
  
  val eval : exp -> value Env.t -> value

  (* test *)
  module Test : sig
    val run_tests : unit -> unit
  end

end = struct

  (* env *)
  module Env = struct
    type 'a t = (string * 'a) list

    let empty_env =
      []

    let extend_env id value env =
      (id, value) :: env

    let extend_env_rec fn_id var_id fn_value env =
      extend_env fn_id fn_value env

    let rec search_var id env =
      match env with
      | [] -> None
      | (var', val') :: xs when var' = id -> Some val'
      | x :: xs -> search_var id xs

    let search_var_exn id env =
      match search_var id env with
        | None -> 
          failwith (
            Printf.sprintf "empty env or has no binding for variable: %s." id
          )
        | Some value -> value

    let apply_env = search_var_exn

  end

  (* lang *)

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
    | Abs of id * exp
    | LetRec of id * id * exp * exp
    | App of exp * exp

  type value =
    | Int of int
    | Bool of bool
    | Fn of id * exp

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
    | Fn (var, body) -> var, body
    | _ -> failwith "must be of type Fn."

  let rec eval exp env =

    let int_binary_op e1 e2 env op =
      let ev e = eval e env in
      Int (op (int_of_val (ev e1)) (int_of_val (ev e2)))
    in

    let apply fn arg =
      let var, body = fn_of_val fn in
      let new_env = Env.extend_env var arg env in
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
      if (bool_of_val (eval pred env))
      then (eval then_exp env)
      else (eval else_exp env)

    | Let (var, bind, body) ->
      let var_value = eval bind env in
      let new_env = Env.extend_env var var_value env in
      eval body new_env

    | Abs (var, body) ->
      Fn (var, body)

    | LetRec (fn_var, var, fn_body, body) ->
      let fn = Fn (var, fn_body) in
      let new_env = Env.extend_env fn_var fn env in
      eval body new_env

    | App (rator, rand) ->
      apply (eval rator env) (eval rand env)

  (* test *)
  module Test = struct

    (* let f = proc (x) -(x,11) in (f (f 77)) ==> Int 55 *)
    let exp1 = Let (
        "f",
        Abs ("x", Sub (Id "x", IntLit 11)),
        App (Id "f", App (Id "f", IntLit 77))
      )

    (* (proc (f) (f (f 77)) proc (x) -(x, 11)) ==> Int 55 *)
    let exp2 = Let (
        "f",
        Abs ("f", App (Id "f", App (Id "f", IntLit 77))),
        Let (
          "g",
          Abs (("x", Sub (Id "x", IntLit 11))),
          App (Id "f", Id "g")
        )
      )

    (* 
      let rec double x =
        if x = 0 then 0 else double (x - 1) + 2
      in double 6
      ==> Int 12 
    *)
    let exp3 = LetRec (
      "double",
      "x",
      If (
        IsZero (Id "x"),
        IntLit 0,
        Add (
          App (Id "double", Sub (Id "x", IntLit 1)),
          IntLit 2
        )
      ),
      App (Id "double", IntLit 6)
    )

    let env0 = Env.empty_env

    let run_tests () =
      Printf.printf "runing LetRecLang test...\n" ;
      begin
        (assert (eval exp1 env0 = Int 55)) ;
        (assert (eval exp2 env0 = Int 55)) ;
        (assert (eval exp3 env0 = Int 12))
      end

  end

end
;;

LetRecLang.Test.run_tests () ;;
