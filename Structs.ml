open Identifier


module H = Hashtbl.Make (
	struct
		type t = id
		let equal = (==)
		let hash = Hashtbl.hash
	end
)


module IntMap = Map.Make(
        struct
                type t = int
                let compare = compare
        end
)

module StringMap = Map.Make(
	struct
		type t = string
		let compare = compare
	end
)

module StringSet = Set.Make(
	struct 
		type t = string
		let compare = compare 
	end
)


module IntSet = Set.Make(
	struct
		type t = int
		let compare = compare
	end
)


module QuadrupleSet = Set.Make(
	struct
		type t = int option * int * int * int
		let compare = compare
	end
)


module QuadrupleMap = Map.Make(
	struct
		type t = int option * int * int * int
		let compare = compare
	end
)


module TripleSet = Set.Make(
	struct
		type t = int * int * int
		let compare = compare
	end
)


module TripleMap = Map.Make(
	struct
		type t = int * int * int
		let compare = compare
	end
)


module TupleSet = Set.Make(
	struct
		type t = int * int
		let compare = compare
	end
)

module FloatMap = Map.Make(
	struct
		type t = float
		let compare = compare
	end
)


