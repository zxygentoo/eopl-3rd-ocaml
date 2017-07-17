open Core.Std

(* 1.1.1 *)
(* int -> bool *)
let rec inS x =
	if x < 0
	then false
	else if x == 0
	then true
	else inS(x - 3)
;;

assert (inS(-2) != false);;
assert (inS(-3) == false);;
assert (inS(-1) == false);;
assert (inS(0) == true);;
assert (inS(1) == false);;
assert (inS(2) == false);;
assert (inS(3) == true);;
assert (inS(6) == true);;

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
	let rec str_lst = function
			| [] ->
				""
			| x::xs ->
				string_of_int x ^ "; " ^ str_lst xs

	and trim_end inner =
		let len = String.length inner in
			if len < 2 then inner
			else String.slice inner 0 (len - 2)

	in
		"[" ^ (lst |> str_lst |> trim_end) ^ "]"
;;

let nth lst n =
	let rec inner_nth lst n =
		match lst with
		| [] ->
			raise err
		| x::xs ->
			if phys_equal n 0 then x else inner_nth xs (n - 1)

	and err = Exn (Printf.sprintf "list %s doesn't have %d elements." (string_of_list lst) n)

	in
		inner_nth lst n
;;

(* 1.2.3 *)

let rec remove_first elem lst =
	match lst with
	| [] ->
		raise Exn "Error"
	| x::xs ->
		if x == elem then xs
		else x::(remove_first elem xs)
;;

(* e1.9 *)

let rec remove elem lst =
	match lst with
	[] ->
		[]
	| x::xs ->
		if phys_equal x elem then remove elem xs
		else x::(remove elem xs)
;;

(* 1.2.4 *)

