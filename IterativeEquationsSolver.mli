val worklist_iterate :
  Structs.IntMap.key array ->
  int ->
  (int -> 'a -> 'b) ->
  int list array ->
  int list array ->
  'a array -> 'a -> 'a * ('a -> 'b -> 'a) -> ('a -> 'a -> bool) -> unit
