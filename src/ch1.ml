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

type lcexp =
  | Var of string
  | Abs of string * lcexp
  | App of lcexp * lcexp
;;

let rec occurs_free sym exp =
  match exp with
  | Var s ->
    sym = s
  | Abs (s, expr) ->
    sym <> s && occurs_free sym expr
  | App (expr1, expr2) ->
    occurs_free sym expr1 || occurs_free sym expr2
;;

let e1 = Var "x";;
let e2 = Abs ("x", e1);;
let e3 = App (e2, Var "c");;
let e4 = App (e2, e3);;

assert (occurs_free "x" e1 = true);;
assert (occurs_free "x" e2 = false);;
assert (occurs_free "x" e3 = false);;
assert (occurs_free "c" e3 = true);;
assert (occurs_free "x" e4 = false);;
assert (occurs_free "c" e4 = true);;

(* 1.2.5 *)

type s_exp =
  | Symbol of string
  | SList of s_list
and s_list = 
  | Empty
  | List of s_exp * s_list
;;

let subst sym_new sym_old slst =
  let rec subst_lst = function
    | Empty ->
      Empty
    | List (expr, lst) ->
      List (subst_exp expr, subst_lst lst)

  and subst_exp = function
    | (Symbol s) as sym ->
      if sym = sym_old then sym_new else sym
    | SList lst ->
      SList (subst_lst lst)
  in
    subst_lst slst
;;

(* 1.3 *)

let number_elements lst =
  let rec number_elements_aux lst n =
    match lst with
    | [] ->
      []
    | x::xs ->
      (n, x)::number_elements_aux xs (n + 1)
  in
    number_elements_aux lst 0
;;

let rec list_sum lst =
  match lst with
  | [] ->
    0
  | x::xs ->
    x + list_sum xs
;;

(* e1.15 *)
let rec duple n item =
  match n with
  | 0 ->
    []
  | _ ->
    item::duple (n - 1) item
;;

(* e1.16 *)
let invert lst =
  List.map lst (fun (a, b) -> (b, a))
;;

(* e1.18 *)
let swapper sym1 sym2 slst =
  let rec swap_lst = function
    | Empty ->
      Empty
    | List (expr, lst) ->
      List (swap_exp expr, swap_lst lst)

  and swap_exp = function
    | (Symbol _) as sym ->
      if sym = sym2 then sym1
      else if sym = sym1 then sym2
      else sym
    | SList lst ->
      SList (swap_lst lst)
  in
    swap_lst slst
;;

(* e1.19 *)
let list_set lst index item =
  let rec list_set_aux n l1 l2 =
    match l2 with
    | [] ->
      l1
    | x::xs ->
      if n = 0 then List.append l1 (item::xs)
      else list_set_aux (n - 1) (List.append l1 [x]) xs
  in
    list_set_aux index [] lst
;;

(* e1.34 *)
type bst =
  | Empty
  | Tree of (int * bst * bst)
;;

type direction =
  | Left
  | Right
;;

let rec item_in_bst item tree =
  match tree with
  | Empty ->
    false
  | Tree (i, left, right) ->
    if i = item then true
    else item_in_bst item left || item_in_bst item right
;;

let path item tree =
  let rec path_aux current_path subtree =
    match subtree with
    | Empty ->
      []
    | Tree (i, left, right) ->
      if i = item then
        current_path
      else if (item_in_bst item left) then
        path_aux (List.cons Left current_path) left
      else if (item_in_bst item right) then
        path_aux (List.cons Right current_path) right
      else []
  in
    List.rev (path_aux [] tree)
;;
