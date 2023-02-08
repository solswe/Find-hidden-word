(* An interface for sets. 

   Please do not change the contents of this file.
 *)

module type SetS = sig

  type 'a t

  val empty : 'a t

  val insert : 'a -> 'a t -> 'a t

  val elem : 'a -> 'a t -> bool

end
