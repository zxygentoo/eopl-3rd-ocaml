open Core.Std

(* 1.1.1 *)

let rec in_s x =
  match x with
  x when x < 0 ->
    false
  | 0 ->
    true
  | _ ->
    in_s(x - 3)
;;

assert (in_s (-3) = false);;
assert (in_s 0 = true);;
assert (in_s 3 = true);;

(* 1.2.1 *)

let rec list_length = function
  | [] -> 
    0
  | x::xs ->
    1 + list_length(xs)
;;

exception Exn of string
;;

(* 1.2.2 *)

(* e1.6 *)
let string_of_list lst =
  let rec string_of_list' = function
      | [] ->
        ""
      | x::xs ->
        string_of_int x ^ "; " ^ string_of_list' xs

  and trim_end lst_str =
    let len = String.length lst_str in
      if len < 2 then lst_str
      else String.slice lst_str 0 (len - 2)

  in
    "[" ^ (lst |> string_of_list' |> trim_end) ^ "]"
;;

let nth lst n =
  let rec nth' lst n =
    match lst with
    | [] ->
      raise err
    | x::xs ->
      if n = 0 then x else nth' xs (n - 1)

  and err = Exn (
    Printf.sprintf  "list %s doesn't have %d elements."
                    (string_of_list lst) n
  )

  in
    nth' lst n
;;

(* 1.2.3 *)

let rec remove_first elem lst =
  match lst with
  | [] ->
    raise (Exn "Element not in list.")
  | x::xs ->
    if x = elem then xs
    else x::(remove_first elem xs)
;;

(* e1.9 *)
let rec remove elem lst =
  match lst with
  [] ->
    []
  | x::xs ->
    if x = elem then remove elem xs
    else x::(remove elem xs)
;;

(* 1.2.4 *)

