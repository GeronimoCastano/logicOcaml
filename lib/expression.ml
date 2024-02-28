type exp =
  | Var of string
  | Bcon of bool
  | And of exp * exp
  | Or of exp * exp
  | Xor of exp * exp
  | Implies of exp * exp
  | Iff of exp * exp
  | Not of exp

(** Evaluates the expression with the given environment (variable-boolean assignments) to a boolean value. 
    Raises [Not_found] if a variable is not found in the environment. 
*)
let rec eval env exp =
  match exp with
  | Var s -> List.assoc s env
  | Bcon b -> b
  | And (e1, e2) 
  | Or (e1, e2)
  | Xor (e1, e2)
  | Implies (e1, e2)
  | Iff (e1, e2) -> 
      let v1, v2 = (eval env e1, eval env e2) in
      (match exp with
      | And _ -> v1 && v2
      | Or _ -> v1 || v2
      | Xor _ -> v1 <> v2
      | Implies _ -> not v1 || v2
      | Iff _ -> v1 = v2
      | _ -> assert false)
  | Not e -> not (eval env e)

(** Returns a list of all the unique variables in the expression. *)
let list_vars exp =
  let rec get_vars exp =
    match exp with
    | Var x -> [ x ]
    | Bcon _ -> []
    | And (e1, e2)
    | Or (e1, e2)
    | Xor (e1, e2)
    | Implies (e1, e2)
    | Iff (e1, e2) ->
        get_vars e1 @ get_vars e2
    | Not e -> get_vars e
  in
  List.sort_uniq compare (get_vars exp)