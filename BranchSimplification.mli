val branchSimplifications :
  (Structs.IntMap.key * Quads.quad) array array * 'a * 'b ->
  Structs.IntMap.key ->
  (Structs.IntSet.elt * Quads.quad) array array * int list array *
  int list array
val removeNextQuadBranches : (int * Quads.quad) array array * 'a * 'b -> unit
