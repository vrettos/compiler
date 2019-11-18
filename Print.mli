val show_offsets : bool
val printSymbolTable : Format.formatter -> unit
val printQuads :
  Format.formatter -> (Structs.IntMap.key * Quads.quad) list list -> unit
val printAsm : Format.formatter -> Asm.line list -> unit
