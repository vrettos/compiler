type typ =
  | TYPE_unit
  | TYPE_bool
  | TYPE_char
  | TYPE_int
  | TYPE_float
  | TYPE_array of typ * int
  | TYPE_ref of typ
  | TYPE_func of typ * typ
  | TYPE_id of string
  | TYPE_var of int


let sizeOfType t =
	match t with
	| TYPE_unit ->
		0
	| TYPE_bool ->
		1
	| TYPE_char ->
		1
	| TYPE_int ->
		2
	| TYPE_float ->
		10
	| TYPE_array _ ->
		2
	| TYPE_ref _ ->
		2
	| TYPE_func _ ->
		2
	| TYPE_id _ ->
		0
	| TYPE_var _ ->
		0


let typeVarNumber = ref 0


let newTypeVar () =
	incr typeVarNumber;
	TYPE_var !typeVarNumber


let rec pretty_typ ppf typ =
	match typ with
	| TYPE_int ->
		Format.fprintf ppf "int"
	| TYPE_float ->
		Format.fprintf ppf "float"
	| TYPE_bool ->
		Format.fprintf ppf "bool"
	| TYPE_unit ->
		Format.fprintf ppf "unit"
	| TYPE_char ->
		Format.fprintf ppf "char"
	| TYPE_ref t ->
		Format.fprintf ppf "(%a) ref" pretty_typ t
	| TYPE_array (t, sz) ->
		let rec print_stars ppf sz =
			match sz with
			| 1 ->
				()
			| _ ->
				Format.fprintf ppf "*,"; print_stars ppf (sz - 1)
		in
		Format.fprintf ppf "array [";
		print_stars ppf sz;
		Format.fprintf ppf "*] of (%a)" pretty_typ t
	| TYPE_func (t1, t2) ->
		Format.fprintf ppf "(%a -> %a)" pretty_typ t1 pretty_typ t2
	| TYPE_id name ->
		Format.fprintf ppf "%s" name
	| TYPE_var i ->
		Format.fprintf ppf "@@%d" i



