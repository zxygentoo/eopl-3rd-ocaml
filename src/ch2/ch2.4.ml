(* variant definition *)

type variant_type =
  {
    type_name : var_name;
    type_pred : fun_name;
    variants : variant list
  }
and variant =
  {
    variant_name : var_name;
    variant_pred : fun_name;
    etors : extractor list
  }
and extractor =
  {
    etor_name : var_name;
    etor_pred : fun_name
  }
and var_name = string
and fun_name = string
;;

(* e2.26 *)

(*
Red-blue-tree     ::= Red-blue-subtree
Red-blue-subtree  ::= (red-node Red-blue-subtree Red-blue-subtree)
                  ::= (blue-node {Red-blue-subtree}∗)
                  ::= (leaf-node Int)
*)

type rbtree = red_blue_subtree
and red_blue_subtree =
  | Red of red_blue_subtree * red_blue_subtree
  | Blue of red_blue_subtree list
  | Leaf of int
;;

(* let rec replace_leaf_with_path_red_nodes_count rbt =
  let rec 
  function
    | Red (t1, t2) ->
      false
    | Blue tree ->
      false
    | Leaf i ->
      false
;; *)

