type id = string

type exp =
  (* Literual types *)
  | IntLit of int
  | BoolLit of bool
  (* abstraction *)
  | Abs of (id * exp)
  (* basic operations *)
  | IsZero of exp
  | Add of (exp * exp)
  | Sub of (exp * exp)
  (* branching *)
  | IF of (exp * exp * exp)
  (* new scope *)
  | Let of (id * exp * exp)
  | LetRec of (id * id * exp * exp)
  (* application *)
  | App of (exp * exp)

type value =
  | Int of int
  | Bool of bool
  | Fn of (id * exp)

module Env : sig
  type t
  val apply : id -> t -> value
  val extend : id -> value -> t -> t

end = struct
  (* Generic Map with String keys *)
  module Env =
    Map.Make (
      struct
        type t = string
        let compare = compare
      end
    )

  type t = {
    current : value Env.t ;
    outer : t option
  }

  let new_scope =
    Env.empty

  let empty =
    {
      current = new_scope ;
      outer = None
    }

  let add_scope env =
    {
      current = new_scope ;
      outer = env
    }

  let extend_scope id value scope =
    Env.add id value scope

  let search_scope id scope =
    Env.find_opt id scope

  let rec search_id id env =
    let { current = current ; outer = outer } = env in
    match search_scope id current with
    | Some _ as res -> res
    | None -> (
      match outer with
      | None -> None
      | Some env -> search_id id env
    )

  let apply id env =
    match search_id id env with
    | Some value -> value
    | None -> failwith (
        Printf.sprintf "Empty environment or no binding for %s." id
    )

  let extend id value env =
    let { current = current ; outer = outer } = env in
    {
      current = extend_scope id value current ;
      outer = outer
    }

  let extend_new_scope id value env =
    {
      current = extend_scope id value new_scope ;
      outer = env
    }

end

(* module type LangType = sig
  type exp
  type value
  val eval : exp -> env -> value
end

let int_of_value =
  function
  | Int x -> x
  | _ -> failwith "must be of type Int."

let bool_of_value =
  function
  | Bool x -> x
  | _ -> failwith "must be of type Bool."

let fn_of_value =
  function
  | Fn (var, body) -> (var, body)
  | _ -> failwith "must be of type Fn."

let rec eval exp env =
  match exp with
  | IntLit x -> Int x
  | BoolLit x -> Bool x
  (* | Abs (id, exp) -> *)

 *)