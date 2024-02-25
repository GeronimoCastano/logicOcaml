open Lexer
open Expression

let rec parser ts = 
  parse_iff ts
  and parse_iff ts = 
    let lhs, ts = parse_implies ts in
    match ts with
    | IFF :: ts ->
      let rhs, ts = parse_iff ts in
        Iff(lhs, rhs), ts
    | _ -> lhs, ts 
  and parse_implies ts = 
    let lhs, ts  = parse_or ts in
    match ts with
    | IMPLIES :: ts ->
      let rhs, ts = parse_implies ts in
        Implies(lhs, rhs), ts
    | _ -> lhs, ts
  
  and parse_or ts = 
    let lhs, ts = parse_xor ts in
    match ts with
    | OR :: ts ->
      let rhs, ts = parse_or ts in
        Or(lhs, rhs), ts
    | _ -> lhs, ts
  and parse_xor ts = 
    let lhs, ts = parse_and ts in
    match ts with
    | XOR :: ts ->
      let rhs, ts = parse_xor ts in
        Xor(lhs, rhs), ts
    | _ -> lhs, ts
  and parse_and ts = 
    let lhs, ts = parse_not ts in
    match ts with
    | AND :: ts ->
      let rhs, ts = parse_and ts in
        And(lhs, rhs), ts
    | _ -> lhs, ts
  and parse_not ts = 
    match ts with
    | NOT :: ts ->
      let e, ts = parse_primary ts in
      Not(e), ts
    | _ -> parse_primary ts
  and parse_primary ts =
    match ts with
    | VAR(v) :: ts -> (Var(v), ts)
    | BCON b :: ts -> (Bcon(b), ts)
    | LP :: ts ->
      let e, ts = parser ts in
      (match ts with 
      | RP :: ts -> (e, ts)
      | _ -> failwith "expected ')'")

    | _ -> failwith "expected token";;