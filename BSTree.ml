
(* Below is a signature for binary search trees that exposes some
   implementation details to facilite testing.
 *)

module type  BSTreeImplS = sig
  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  include BSTreeSig.BSTreeS with type 'a t = 'a tree

  (* You may add additiona functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_bst` may use a function 
     `min_tree` and thus add the following to this signature:

     val min_tree : 'a tree -> 'a option

     You may add what ever functions you like here. Then add
     tests for them in `BSTreeSet_Tests_Impl.ml`
   *)

end


module BSTreeImplM : BSTreeImplS = struct

  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty : 'a t = Leaf

  let rec insert (e: 'a) (t: 'a t) : 'a t =
    match t with
    | Leaf -> Fork (Leaf, e, Leaf)
    | Fork (l, v, r) -> if e < v then Fork (insert e l, v, r)
                        else Fork (l, v, insert e r)

  let rec elem (e: 'a) (t: 'a t) : bool = 
    match t with
    | Leaf -> false
    | Fork (l, v, r) -> e = v || (e < v && elem e l) 
                              || (e > v && elem e r)

  let rec height (t: 'a t) : int =
    match t with 
    | Leaf -> 0
    | Fork (l, v, r) -> 1 + (max (height l) (height r))

  let rec size (t: 'a t) : int =
    match t with 
    | Leaf -> 0
    | Fork (l, v, r) -> 1 + size l + size r

  (* Find minimum value in a tree*)
  let rec min_tree (t: 'a t) : 'a option = 
    match t with 
    | Leaf -> None
    | Fork (l, v, r) -> match (min_tree l, min_tree r) with
                        | (None, None) -> Some v
                        | (None, Some x) | (Some x, None) -> Some (min v x)
                        | (Some x, Some y) -> Some (min x (min v y))
                        
  let rec is_bst (t: 'a t) : bool = 
    match t with
    | Leaf -> true
    | Fork (l, v, r) -> 
      match (is_bst l, is_bst r) with
      | (false, _) | (_, false) -> false 
      | (true, true) -> let left_min = min_tree l 
                        in let right_min = min_tree r
                        in match (left_min, right_min) with
                           | (None, Some y) -> if v < y then true else false
                           | (Some x, None) -> if x < v then true else false 
                           | (Some x, Some y) -> if x < v && v < y then true else false
                           | _ -> true

end



(* Below we create a new moduled `BSTreeM` that only exposes the
   binary-search tree functionality in the BSTreeS` signature.
   Functions that may be useful for testing, such as `tree_min`
   are not accessible in `BSTreeM`.

   We "seal" `BSTreeM` with the signature `BSTreeS` so that it only
   exposes the elements of `BSTreeS`.
 *)
module BSTreeM : BSTreeSig.BSTreeS = BSTreeImplM



(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `TreeM` are not
   accessible in `TreeSetM`.

   We "seal" `TreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module BSTreeSetM : SetSig.SetS = BSTreeM


