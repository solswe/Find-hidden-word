(* An interface for binary search trees. 

   This includes the signature for sets and adds a
   few functions specific to trees.

   Please do not change the contents of this file.
 *)

module type BSTreeS = sig

  include SetSig.SetS

  (* A size function to count the number of values stored in
     a tree. *)
  val height: 'a t -> int

  (* A size function to count the number of values stored in
     a tree. *)
  val size: 'a t -> int

   (* A function find the minumum value in a tree *)
  val min_tree : 'a t -> 'a option

  (* A function to check that the tree has the structure of a
     binary search tree. That is, all values smaller than the
     value on a `Fork` node are in the left sub-tree and all 
     values larger are in the right sub-tree, and this property
     holds for all `Fork` nodes in the tree. This function may
     be used in Problem 8 of Hwk 05. *)
  val is_bst: 'a t -> bool

end
