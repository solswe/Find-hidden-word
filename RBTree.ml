(* Below is a signature for red-black trees that exposes some
   implementation details to facilitate testing.
 *)

module type  RBTreeImplS = sig

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  include RBTreeSig.RBTreeS with type 'a t = 'a tree

  (* You may add additiona functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_red_black_tree` may use helper
     function and you may add them here to facilitate testing.
   *)

end

module RBTreeImplM : RBTreeImplS = struct

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty : 'a t = E

  let balance (c: color) (l: 'a t) (v: 'a) (r: 'a t) : 'a t =
    match c, l, v, r with
    | B, T(R, T(R, a, x, b), y, c), z, d 
    | B, a, x, T(R, b, y, T(R, c, z, d))
    | B, T(R, a, x, T(R, b, y, c)), z, d
    | B, a, x, T(R, T(R, b, y, c), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    | color, left, e, right -> T(color, left, e, right)

  let rec insert (e: 'a) (t: 'a t) : 'a t =
    let rec ins (tt: 'a t) : 'a t =
      match tt with
      | E -> T (R, E, e, E)
      | T (color, l, v, r) -> 
          if e < v then balance color (ins l) v r
          else if v < e then balance color l v (ins r)
          else tt
    in 
    match ins t with
    | E -> failwith "Cannot happen"
    | T (color, l, v, r) -> T (B, l, v, r)
      
  let rec elem (e: 'a) (t: 'a t) : bool =
    match t with
    | E -> false
    | T (color, l, v, r) -> if e < v then elem e l
                            else if v < e then elem e r
                            else true

  let rec height (t: 'a t) : int =
    match t with
    | E -> 0
    | T (color, l, v, r) -> 1 + max (height l) (height r)

  let rec size (t: 'a t) : int = 
    match t with
    | E -> 0
    | T (color, l, v, r) -> 1 + size l + size r
  
  let rec min_tree (t: 'a t) : 'a option = 
    match t with
    | E -> None
    | T (color, l, v, r) -> match (min_tree l, min_tree r) with
                            | (None, None) -> Some v
                            | (None, Some x) | (Some x, None) -> Some (min v x)
                            | (Some x, Some y) -> Some (min x (min v y))

  let rec is_bst (t: 'a t) : bool = 
    match t with
    | E -> true
    | T (color, l, v, r) -> 
      match (is_bst l, is_bst r) with
      | (false, _) | (_, false) -> false 
      | (true, true) -> let left_min = min_tree l
                        in let right_min = min_tree r
                        in match (left_min, right_min) with
                           | (None, Some y) -> if v < y then true else false
                           | (Some x, None) -> if x < v then true else false 
                           | (Some x, Some y) -> if x < v && v < y then true else false
                           | _ -> true    

  let check_color (t: 'a t) : color = 
    match t with
    | E -> B
    | T(c, l, r, v) -> c


  (* find the number of black nodes of the longest path in a red-black tree *)
  let rec longestpath (t: 'a t) : int =
    let rec count_blacknode (t: 'a t) : int = 
      match t with
      | T (B, l, v, r) -> 1 + (if count_blacknode l > count_blacknode r 
                              then count_blacknode l else count_blacknode r)
      | T (R, l, v, r) -> (if count_blacknode l > count_blacknode r 
                          then count_blacknode l else count_blacknode r)
      | E -> 1
  in match t with 
     | E -> 1
     | T (color, l, v, r) -> let left_b = count_blacknode l
                             in let right_b = count_blacknode r
                             in if left_b >= right_b then left_b else right_b


  (* find the number of black nodes of the shortest path in a red-black tree *)
  let rec shortestpath (t: 'a t) : int =
    let rec count_blacknode (t: 'a t) : int = 
      match t with
      | T (B, l, v, r) -> 1 + (if count_blacknode l < count_blacknode r 
                               then count_blacknode l else count_blacknode r)
      | T (R, l, v, r) -> (if count_blacknode l < count_blacknode r 
                        then count_blacknode l else count_blacknode r)
      | E -> 1
  in match t with 
     | E -> 1
     | T (color, l, v, r) -> let left_b = count_blacknode l
                             in let right_b = count_blacknode r
                             in if left_b <= right_b then left_b else right_b

                             
  let rec is_red_black_tree (t: 'a t) : bool =
    match is_bst t with
    | false -> false
    | true ->
        match t with
         | E -> true 
         | T (color, l, v, r) -> if color = R && (check_color l = R || check_color r = R)
                                 then false 
                                 else if (l = E && r = E) then true
                                 else if (l = E && r <> E) || (l <> E && r = E) then false 
                                 else (if shortestpath t = longestpath t then true else false)
        

end



(* Below we create a new modules `RBTreeM` that only exposes the
   red-black tree functionality in the `RBTreeS` signature.
   Functions that may be useful for testing, such as `all_paths`
   are not accessible in `RBTreeM`.

   We "seal" `RBTreeM` with the signature `RBTreeS` so that it only
   exposes the elements of `RBTreeS`.   
*)
module RBTreeM : RBTreeSig.RBTreeS = RBTreeImplM



(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeBSTM : BSTreeSig.BSTreeS = RBTreeImplM



(* Below we create a new module `RBTreeSetM` that only exposes the
   set functionality in the `SetSig` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeSetM : SetSig.SetS = RBTreeImplM
