module type HiddenS = sig
  val hidden_words : string list -> (string * string * string) list
end
