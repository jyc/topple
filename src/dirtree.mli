type tree = 
  | Directory of string * tree list
  | File of string

val fold : ('a -> tree -> 'a) -> 'a -> tree -> 'a
val top : tree -> string

val trees
  :  ?depth : int
  -> ?all_dirs : bool
  -> pred : (string -> bool)
  -> string
  -> tree list
