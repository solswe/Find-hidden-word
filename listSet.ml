open SetSig

(* Below add you implementation of sets as lists. Since these are
   pretty simple we do not build a separate version for testing 
   like we see in `BSTreeSet.ml` or `RBTreeSet.ml`.
 *)

module ListSetM : SetS = struct

  type 'a t = 'a list

  let empty : 'a t = []

  let rec insert (e: 'a) (l: 'a t) : 'a t =
    match l with
    | [] -> [e]
    | x :: xs when e < x -> e :: x :: xs
    | x :: xs -> x :: (insert e xs)

  let rec elem (e: 'a) (l: 'a t) : bool =
    match l with
    | [] -> false
    | x :: xs -> e = x || (e > x && elem e xs)
end
