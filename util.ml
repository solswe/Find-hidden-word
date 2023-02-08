module UtilM = struct

  (* Convert char list into string *)
  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)

  (* Convert string into char list *)
  let explode (s: string) : char list =
    let len = String.length s in
    let rec f i = if i = len then [] else s.[i] :: f (i+1) in
    f 0

  let read_words (file_name: string) : string list =
    let ic = open_in file_name in
    let rec read_lines ic = try
        let next_line = input_line ic in
        next_line :: read_lines ic
      with _ -> []
    in
    let raw_strings = read_lines ic 
    in
    List.filter (fun s -> String.length s > 0)
      (List.map String.trim raw_strings)

  let rec print_answers (ans: (string * string * string) list) =
    match ans with
    | [] -> ()
    | (w3,w5,w8) :: rest -> 
       print_endline ( "(" ^ w3 ^ ", " ^ w5 ^ ", " ^ w8 ^ ")" );
       print_answers rest

end
