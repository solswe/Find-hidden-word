open SetSig
open Hidden_Words_Sig
open Util


(* Here, we choose to not specify a "implementation" signature like
   `BSTreeImplS` one defined in `BSTree.ml`. Instead we leave the
   result of the functor to be "open" - that is, not sealed by any
   signature.
*)
module HiddenWordsImplF (S: SetS)  = struct

  (* There may be other helper functions defined here. *)

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

  (* Convert char list into string *)
  let implode (cs: char t) : string =
    String.concat "" (List.map  (String.make 1) cs)

  (* Convert string into char list *)
  let explode (s: string) : char t =
    let len = String.length s in
    let rec f i = if i = len then [] else s.[i] :: f (i+1) in
    f 0

  (* Find only three letter words *)
  let three_letter_words (wordslist: string t) : string t =
    match wordslist with
    | [] -> []
    | x :: xs -> List.filter (fun x -> String.length x = 3) wordslist

  (* Find only five letter words *)
  let five_letter_words (wordslist: string t) : string t =
    match wordslist with
    | [] -> []
    | x :: xs -> List.filter (fun x -> String.length x = 5) wordslist
  
  (* Find only eight letter words *)
  let eight_letter_words (wordslist: string t) : string t =
    match wordslist with
    | [] -> []
    | x :: xs -> List.filter (fun x -> String.length x = 8) wordslist

  (* Remove tuple containing empty list *)
  let rec remove_emptylist (w: ('a t * 'a t) t) : ('a t * 'a t) t =
    match w with
    | [] -> []
    | (x, xs) :: y -> if List.length x = 0 || List.length xs = 0 
                      then remove_emptylist y
                      else (x, xs) :: remove_emptylist y 

  
  let all_parts (w: 'a list) : ('a t * 'a t) t =    
    let rec helper (w: 'a t) (acc: 'a t) =
      match w with
      | [] -> [(acc, w)]
      | x :: xs -> (helper xs (acc @ [x])) @ [(acc, w)]
    in remove_emptylist (helper w [])

  (* Change strings into characters and execute all_parts *)
  let fiveletters_to_char (wordslist: string t) : (char t * char t) t t = 
    let word_to_char = List.map (fun x -> explode x) wordslist
    in List.map all_parts word_to_char

  (* Transfer char list to string tuples *)
  let rec char_to_string (fiveletters: ('a t * 'a t) t) : (string * string) t =
    match fiveletters with
    | (x, xs) :: y -> (implode x, implode xs) :: char_to_string y
    | _ -> []

  let rec change_by_list (charlist: (char t * char t) t t)
                         : (string * string) t = 
    match charlist with
    | x :: xs -> char_to_string x @ change_by_list xs
    | [] -> []

  (* Use threeletters list and fiveletters list and
     make (threeletter, fiveletter, (three + five letter)) format *)
  let rec combine_thr_and_fiv (threeletters: string t) 
                              (fiveletters: (string * string) t)
                              : (string * string * string) t =
    match threeletters with
    | x :: xs -> List.map (fun (y, ys) -> (x, y ^ ys, y ^ x ^ ys)) 
               fiveletters @ combine_thr_and_fiv xs fiveletters
    | _ -> []

  (* Check if eightletter words made by combine_thr_and_fiv exist in a eightletters list *)
  let rec check_existence (combined: (string * string * string) t)
                          (eightletters: string t)
                          : (string * string * string) t =
    match combined with
    | (thr, fiv, eig) :: rest -> if (elem eig eightletters)  
                                 then (thr, fiv, eig) :: check_existence rest eightletters
                                 else check_existence rest eightletters
    | _ -> []
  
  (* Find hidden words *)
  let hidden_words (word_list: string t) : (string * string * string) t =
    let threeletters = three_letter_words word_list
    in
    let fiveletters = five_letter_words word_list
    in
    let fiveletters_charlist = change_by_list (fiveletters_to_char fiveletters)
    in
    let combined = combine_thr_and_fiv threeletters fiveletters_charlist
    in
    let eightletters = eight_letter_words word_list
    in
    check_existence combined eightletters

        (* raise (Failure "hidden_words not yet implemented.") *)

end


(* Here we dfined the functor `HiddenF`. It uses the `HiddenImplF`
   functor but only exposes the `hidden_words` function that is
   defined in `HiddenS`.

   We will use `HiddenF` in `solution.ml` to create the list-based
   and the tree-based solutions to the problem.
 *)

module HiddenWordsF (S: SetS) : HiddenS = HiddenWordsImplF(S)
