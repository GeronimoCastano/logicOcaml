
type token =
  | AND
  | OR
  | IMPLIES
  | IMPLIESBACK
  | IFF
  | NOT
  | VAR of string
  | BCON of bool
  | LP
  | RP
  | XOR;;


let isLetter c = match c with
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false;;




let isWhitespace c = 
  match c with 
  | ' ' | '\n' | '\t' -> true
  | _ -> false;;


let varOrId s = 
  match s with
  | "T" -> BCON(true)
  | "F" -> BCON(false)
  | _ as id -> VAR(id);;



let lexer s = 
  let get i = String.get s i in
  let n = String.length s in
  let slice i j = String.sub s i (j-i+1) in
  
  let rec loop i ts = 
    if i >= n then List.rev ts else
    match get i with
    | '&' -> loop (i+1) (AND :: ts)
    | '|' -> loop (i+1) (OR :: ts)
    | '-' -> if i + 1 < n && get (i+1) = '>' then loop (i+2) (IMPLIES :: ts) else failwith "expected '>'"
    | '<' -> if i + 1 < n && get (i+1) = '-' then
                if i + 2 < n && get (i+2) = '>' then loop (i+3) (IFF :: ts)
                else loop (i+2) (IMPLIESBACK :: ts)
              else failwith "expected '-'"
    | '^' -> loop (i+1) (XOR :: ts)
    | '~' -> loop (i+1) (NOT :: ts)
    | '(' -> loop (i+1) (LP :: ts)
    | ')' -> loop (i+1) (RP :: ts)
    | c when isWhitespace c -> loop (i+1) ts
    | c when isLetter c -> lexId i i ts
    | _ -> failwith "illegal character"
  and  lexId i j ts = 
    if j >= n then List.rev (varOrId(slice i (j-1)) :: ts)
    else match get j with
    | c when isLetter c -> lexId i (j+1) ts
    | _ -> loop j (varOrId(slice i (j-1)) :: ts)
  in loop 0 [];;
