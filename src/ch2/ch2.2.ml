(* e2.4 *)

module Stack : sig
  type 'a stack = 'a list
  val empty : 'a stack
  val push : 'a -> 'a stack -> 'a stack
  val pop : 'a stack -> 'a * 'a stack
  val top : 'a stack -> 'a
  val is_empty : 'a stack -> bool

end = struct
  type 'a stack = 'a list

  let empty = []

  let push a s =
    a::s

  let pop s =
    match s with
    | [] ->
      failwith "Empty stack."
    | x::xs ->
      x, xs

  let top s =
    match s with
    | [] ->
      failwith "Empty stack."
    | x::xs ->
      x

  let is_empty s =
    s = []

end
;;

(* tests *)

assert (Stack.empty = []);;
assert (Stack.empty |> Stack.is_empty = true);;
assert (Stack.empty |> Stack.push 1 = [1]);;
assert (Stack.empty |> Stack.push 1 |> Stack.is_empty = false);;
assert (Stack.empty |> Stack.push 1 |> Stack.top = 1);;
assert (Stack.empty |> Stack.push 1 |> Stack.push 2 |> Stack.top = 2);;
assert (Stack.empty |> Stack.push 1 |> Stack.pop = (1, []));;
assert (Stack.empty |> Stack.push 1 |> Stack.push 2 |> Stack.pop = (2, [1]));;


(* e2.5/2.7/2.8/2.9/2.10 *)
(* assume var as string and val as int for simple sake. *)

module Env : sig
  type env = (string * int) list
  val empty_env : env
  val extend_env : string -> int -> env -> env
  val apply_env : string -> env -> int
  val is_empty_env : env -> bool
  val has_binding : string -> env -> bool
  val extend_env_multiple : (string * int) list -> env -> env
end = struct
  type env = (string * int) list

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

end
;;

(* tests *)
assert (Env.empty_env = []);;
assert (Env.empty_env |> Env.extend_env "var1" 1 = [("var1", 1)]);;
assert (Env.empty_env |> Env.extend_env "var1" 1 |> Env.extend_env "var2" 2 = [("var2", 2); ("var1", 1)]);;
assert (Env.empty_env |> Env.extend_env "var1" 1 |> Env.extend_env "var2" 2 |> Env.apply_env "var1" = 1);;
assert (Env.empty_env |> Env.extend_env "var1" 1 |> Env.extend_env "var2" 2 |> Env.apply_env "var2" = 2);;
Env.empty_env |> Env.apply_env "err: empty-env";;
Env.empty_env |> Env.extend_env "var1" 1 |> Env.apply_env "err: no-binding";;
assert (Env.empty_env |> Env.has_binding "var" = false);;
assert (Env.empty_env |> Env.extend_env "var" 42 |> Env.has_binding "var" = true);;
assert (
  Env.empty_env |> Env.extend_env_multiple [("var1", 42); ("var2", 43)]
  = (Env.empty_env |> Env.extend_env "var1" 42 |> Env.extend_env "var2" 43)
);;


(* eg2.2.1 *)

module ProcEnv : sig
  type env = (string * int) list
  val empty_env : string -> env
  val extend_env : string -> int -> env -> string -> int
  val apply_env : env -> string -> int
end = struct
  type env = (string * int) list

  let rec search_var' env s =
    match env with
    | [] ->
      None
    | (var', val')::xs when var' = s ->
      Some val'
    | x::xs ->
      search_var' xs s

  let search_var env s =
    match search_var' env s with
    | None ->
      failwith "Empty enviroment or no binding for variable."
    | Some v ->
      v

  let empty_env =
    search_var []

  let extend_env s v env =
    search_var ((s, v)::env)

  let apply_env =
    search_var

end
;;

(* e2.12 *)

module ProcStack : sig
  type 'a stack = 'a list
  val pop : 'a stack -> unit -> 'a * 'a stack
  val empty : unit -> 'a * 'a list
  val push : 'a -> 'a stack -> (unit -> ('a * 'a stack))
  val top : 'a stack -> unit -> 'a

end = struct
  type 'a stack = 'a list

  let pop'' s =
    match s with
    | [] ->
      None
    | x::xs ->
      Some (x, xs)

  let pop' s =
    match pop'' s with
    | None ->
      failwith "Empty stack."
    | Some (x, xs) ->
      x, xs

  let top' s =
    let (x, _) = pop' s in x

  let pop s =
    fun () -> pop' s

  let empty =
    fun () -> pop' []

  let push a s =
    fun () -> pop' (a::s)

  let top s =
    fun () -> top' s

end
;;

(* test *)

(ProcStack.empty) ();;
(ProcStack.pop []) ();;
(ProcStack.pop [42]) ();;
(ProcStack.pop [42; 43]) ();;
(ProcStack.push 42 []) ();;
(ProcStack.top []) ();;
(ProcStack.top [42]) ();;
