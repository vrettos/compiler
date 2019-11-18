val controlFlowGraph_of_function :
  (Structs.IntSet.elt * Quads.quad) list ->
  (Structs.IntSet.elt * Quads.quad) array array * int list array *
  int list array
val function_of_controlFlowGraph :
  Structs.IntMap.key ->
  (Structs.IntMap.key * Quads.quad) array array * 'a * 'b ->
  Structs.IntMap.key * (Structs.IntMap.key * Quads.quad) list
val updateControlFlowGraph :
  Structs.IntMap.key ->
  (Structs.IntMap.key * Quads.quad) array array * 'a * 'b ->
  (Structs.IntSet.elt * Quads.quad) array array * int list array *
  int list array
val dfsPP :
  int -> int list array -> int -> bool array * int array * int array
