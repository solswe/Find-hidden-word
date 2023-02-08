open SetSig
open Hidden_Words_Sig
open Util

(* This module is meant to include only tail recursive functions.
 *)
module HiddenImpl_TR_F (S: SetS)  = struct

  (* There may be other helper functions defined here. *)

  type 'a t = 'a list

  let empty : 'a t = []

  (* Insert an element e into a list *)
  let insert (e: 'a) (l: 'a t) : 'a t = 
    let rec ins_tr (so_far: 'a t) (e: 'a) (l: 'a t) : 'a t =
      match l with 
      | [] -> so_far @ [e]
      | x :: xs when e < x -> so_far @ e :: x :: xs
      | x :: xs -> ins_tr (x :: so_far) e xs
    in ins_tr [] e l

  (* Check if an element e is in a list*)
  let elem (e: 'a) (l: 'a t) : bool = 
    let rec ele_tr (so_far: bool) (e: 'a) (l: 'a t) : bool =
      match l with
      | [] -> so_far && false
      | x :: xs -> e = x || ele_tr (e > x && so_far) e xs
    in ele_tr true e l

  (* Convert char list into string *)
  let implode (cs: char t) : string =
    String.concat "" (List.map  (String.make 1) cs)

  (* Convert string into char list *)
  let explode (s: string) : char t =
    let len = String.length s in
    let rec f i = if i = len then [] else s.[i] :: f (i+1) 
    in f 0

  (* Find only three letter words *)
  let three_letter_words (wordslist: string t) : string t =
    let rec thr_tr (so_far: string t) (wordslist: string t) : string t = 
      match wordslist with
      | [] -> so_far
      | x :: xs when String.length x = 3 -> thr_tr (x :: so_far) xs 
      | x :: xs -> thr_tr so_far xs 
    in thr_tr [] wordslist

  (* Find only five letter words *)
  let five_letter_words (wordslist: string t) : string t =
    let rec fiv_tr (so_far: string t) (wordslist: string t) : string t = 
      match wordslist with
      | [] -> so_far
      | x :: xs when String.length x = 5 -> fiv_tr (x :: so_far) xs 
      | x :: xs -> fiv_tr so_far xs 
    in fiv_tr [] wordslist
  
  (* Find only eight letter words *)
  let eight_letter_words (wordslist: string t) : string t =
    let rec eig_tr (so_far: string t) (wordslist: string t) : string t = 
      match wordslist with
      | [] -> so_far
      | x :: xs when String.length x = 8 -> eig_tr (x :: so_far) xs 
      | x :: xs -> eig_tr so_far xs 
    in eig_tr [] wordslist

  (* Remove tuple containing empty list *)
  let remove_emptylist (wordslist: ('a t * 'a t) t) : ('a t * 'a t) t =
    let rec rem_tr (so_far: ('a t * 'a t) t) (wordslist: ('a t * 'a t) t) 
                : ('a t * 'a t) t =
      match wordslist with
      | [] -> so_far
      | (x, xs) :: y when (List.length x = 0 || List.length xs = 0) -> rem_tr so_far y 
      | (x, xs) :: y -> rem_tr ((x, xs) :: so_far) y
    in
    rem_tr [] wordslist

  (* Generate all the prefixes and suffixes of a wordlist
     and remove pairs including empty list (ex. (['c'; 'a'; 'r'], []) or ([], ['c'; 'a'; 'r']))
     because they are not useful for this hidden words game. *)
  let all_parts (wordslist: 'a t) : ('a t * 'a t) t =    
    let rec all_tr (acc: 'a t) (wordslist: 'a t) =
      match wordslist with
      | [] -> [(acc, wordslist)]
      | x :: xs -> (all_tr (acc @ [x]) xs) @ [(acc, wordslist)]
    in remove_emptylist (all_tr [] wordslist)


  (* Change strings into characters and execute all_parts *)
  let stringl_to_charl (wordslist: 'a t) : ('b t * 'b t) t t = 
    let rec exp_tr (so_far: 'b t t) (wordslist: 'a t) 
                   : 'b t t  =
      match wordslist with
      | [] -> so_far
      | x :: xs -> exp_tr ((explode x) :: so_far) xs
    in
    let rec str_tr (so_far: ('b t * 'b t) t t) (wordslist: 'b t t) 
                  : ('b t * 'b t) t t = 
      match wordslist with
      | [] -> so_far 
      | x :: xs -> str_tr (all_parts x :: so_far) xs
    in
    str_tr [] (exp_tr [] wordslist)

  (* Transfer char list to string tuples *)
  let char_to_string (charlist: ('a t * 'a t) t) : (string * string) t =
    let rec implode_tr (so_far: (string * string) t) (charlist: ('a t * 'a t) t) : (string * string) t =
      match charlist with
      | (x, xs) :: y -> implode_tr ((implode x, implode xs) :: so_far) y
      | [] -> so_far
    in 
    implode_tr [] charlist

  (* Transfer char to string 
     Compute list by list to change return type as (string * string) t *)
  let change_by_list (charlist: (char t * char t) t t) : (string * string) t = 
    let rec cha_tr (so_far: (string * string) t)
                   (charlist: (char t * char t) t t) : (string * string) t = 
      match charlist with
      | [] -> so_far
      | x :: xs -> cha_tr ((char_to_string x) @ so_far) xs
    in 
    cha_tr [] charlist

  (* Use threeletters list and fiveletters list and
     make (threeletter, fiveletter, (three + five letter)) format *)
  let combine_thr_and_fiv (threeletters: string t) (fiveletters: (string * string) t)
                          : (string * string * string) t =
    let rec com_tr (so_far: (string * string * string) t) (threeletter: string) 
                   (fiveletters: (string * string) t) : (string * string * string) t =
      match fiveletters with
      | [] -> so_far
      | (y, ys) :: x -> 
          com_tr ((threeletter, y ^ ys, y ^ threeletter ^ ys) :: so_far) threeletter x
    in
    let rec thr_tr (acc: (string * string * string) t) (threeletters: string t) 
                   (fiveletters: (string * string) t) : (string * string * string) t =
      match threeletters with
      | [] -> acc
      | x :: xs -> thr_tr ((com_tr [] x fiveletters) @ acc) xs fiveletters
    in thr_tr [] threeletters fiveletters 

  (* Check if eightletter words made by combine_thr_and_fiv exist in a eightletters list *)
  let check_words (combined: (string * string * string) t) (eightletters: string t)
                  : (string * string * string) t =
    let rec che_tr (acc: (string * string * string) t) 
                   (combined: (string * string * string) t) 
                   (eightletters: string t) : (string * string * string) t =
      match combined with
      | (thr, fiv, eig) :: rest when (elem eig eightletters) -> 
          (che_tr ((thr, fiv, eig) :: acc) rest eightletters)
      | (thr, fiv, eig) :: rest -> (che_tr acc rest eightletters)
      | _ -> acc
    in che_tr [] combined eightletters 
  
  (* Find hidden words *)
  let hidden_words (word_list: string t) : (string * string * string) t =
    let threeletters = three_letter_words word_list
    in
    let fiveletters = five_letter_words word_list
    in
    let fiveletters_charlist = change_by_list (stringl_to_charl fiveletters)
    in
    let combined = combine_thr_and_fiv threeletters fiveletters_charlist
    in
    let eightletters = eight_letter_words word_list
    in
   check_words combined eightletters


end


(* Here we dfined the functor `HiddenF`. It uses the `HiddenImplF`
   functor but only exposes the `hidden_words` function that is
   defined in `HiddenS`.

   We will use `HiddenF` in `solution.ml` to create the list-based
   and the tree-based solutions to the problem.
 *)

module HiddenWords_TR_F (S: SetS) : HiddenS = HiddenImpl_TR_F(S)
