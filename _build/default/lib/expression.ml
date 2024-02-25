open Utils

  
type exp = 
| Var of string
| Bcon of bool
| And of exp * exp
| Or of exp * exp
| Xor of exp * exp
| Implies of exp * exp
| ImpliesBack of exp * exp
| Iff of exp * exp
| Not of exp;;


let rec eval env exp = 
  match exp with
  | Var(s) -> lookup s env
  | Bcon(b) -> b
  | And(e1, e2) -> let v1, v2 = eval env e1, eval env e2 in v1 && v2
  | Or(e1, e2) -> let v1, v2 = eval env e1, eval env e2 in v1 || v2
  | Xor(e1, e2) -> let v1, v2 = eval env e1, eval env e2 in
    begin match v1, v2 with
    | true, true -> false
    | false, false -> false
    | true, false -> true
    | false, true -> true end
  | Implies(e1, e2) -> let v1, v2 = eval env e1, eval env e2 in
    begin match v1, v2 with
    | false, false -> true
    | false, true -> true
    | true, false -> false
    | true, true -> true end
  | ImpliesBack(e1, e2) -> let v1, v2 = eval env e1, eval env e2 in
    begin match v1, v2 with
    | false, false -> true
    | false, true -> false
    | true, false -> true
    | true, true -> true end
  | Iff(e1, e2) -> let v1, v2 = eval env e1, eval env e2 in
    begin match v1, v2 with
    | false, false -> true
    | true, true -> true
    | _, _ -> false end
  | Not(e) -> not (eval env e);;


let listVars exp = 
  let rec helper exp = 
  match exp with
  | Var(x) -> [x]
  | Bcon(_) -> []
  | And(e1, e2)
  | Or(e1, e2)
  | Xor(e1, e2)
  | Implies(e1, e2)
  | Iff(e1, e2)
  | ImpliesBack(e1, e2) -> helper e1 @ helper e2
  | Not(e) -> helper e
  in fst (foldl (fun v (res, seen) -> if List.mem v seen then (res, seen) else (v :: res, v :: seen)) (helper exp) ([], []));;

