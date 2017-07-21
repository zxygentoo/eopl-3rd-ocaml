(* e2.17 *)

(* e2.18 *)

module BiIntList : sig
  type bilist = int * int list * int list
  val current : bilist -> int
  val move_left : bilist -> bilist
  val move_right : bilist -> bilist
  val insert_left : int -> bilist -> bilist
  val insert_right : int -> bilist -> bilist
  val at_left_end : bilist -> bool
  val at_right_end : bilist -> bool  

end = struct
  type bilist = int * int list * int list

  let current = function
    | i, _, _ -> i

  let move_left = function
    | _, [], _ -> failwith "Already at left end."
    | current, x::xs, right -> x, xs, current::right

  let move_right = function
    | _, _, [] -> failwith "Already at right end."
    |current, left, x::xs -> x, current::left, xs

  let insert_left i = function
    | current, left, right -> current, i::left, right

  let insert_right i = function
    | current, left, right -> current, left, i::right

  let at_left_end = function
    | _, [], _ -> true
    | _ -> false

  let at_right_end = function
    | _, _, [] -> true
    | _ -> false

end
;;

(* tests *)

let lst1 = (7, [], [])
let lst2 = (6, [5; 4; 3; 2; 1], [7; 8; 9]);;

assert (BiIntList.current lst2 = 6);;
BiIntList.move_left (1, [], [2]);;    (* fail: already at left end .*)
BiIntList.move_right (1, [2], []);;   (* fail: already at right end. *)
assert (BiIntList.move_left lst2 = (5, [4; 3; 2; 1], [6; 7; 8; 9]));;
assert (BiIntList.move_right lst2 = (7, [6; 5; 4; 3; 2; 1], [8; 9]));;
assert (BiIntList.insert_left 1 lst1 = (7, [1], []));;
assert (BiIntList.insert_right 1 lst1 = (7, [], [1]));;

(* e2.19 *)

module BTree : sig
  type btree =
    | Empty
    | Node of (int * btree * btree)

  val current : btree -> int
  val left_branch : btree -> btree
  val right_branch : btree -> btree
  val is_leaf : btree -> bool
  val insert_left : int -> btree -> btree
  val insert_right : int -> btree -> btree

end = struct
  type btree =
    | Empty
    | Node of (int * btree * btree)

  exception EmptyTree

  let current =
    function
      | Empty -> raise EmptyTree
      | Node (i, _, _) -> i

  let left_branch = 
    function
      | Empty -> raise EmptyTree
      | Node (_, left, _) -> left

  let right_branch =
    function
      | Empty -> raise EmptyTree
      | Node (_, _, right) -> right

  let is_leaf =
    function
      | Empty | Node (_, Empty, Empty) -> true
      | _ -> false

  let insert_left i =
    function
      | Empty ->
        raise EmptyTree

      | Node (root, Empty, right) ->
        Node (root, Node (i, Empty, Empty), right)

      | Node (root, left, right) ->
        Node (root, Node (i, left, Empty), right)

  let insert_right i =
  function
    | Empty ->
      raise EmptyTree

    | Node (root, left, Empty) ->
      Node (root, left, Node (i, Empty, Empty))

    | Node (root, left, right) ->
      Node (root, left, Node (i, Empty, right))

end
;;

(* tests *)

let t1 = BTree.Node (
  13, BTree.Node (12, BTree.Empty, BTree.Empty), BTree.Node (14, BTree.Empty, BTree.Empty)
);;

BTree.current BTree.Empty;;     (* fail: empty tree *)
assert (BTree.current t1 = 13);;
assert (t1 |> BTree.left_branch |> BTree.right_branch |> BTree.is_leaf = true);;
assert (
  BTree.insert_left 15 t1 =
  BTree.Node (13,
    BTree.Node (15, BTree.Node (12, BTree.Empty, BTree.Empty), BTree.Empty),
    BTree.Node (14, BTree.Empty, BTree.Empty)
  )
);;

(* e2.20 *)
