open Parser
open Lexer
open Expression

let code s = s |> lexer |> parser |> fst |> eval []

let addOne bin =
  let rec helper bin =
    match bin with
    | [] -> [ true ]
    | h :: t -> if h then false :: helper t else true :: t
  in
  List.rev (helper (List.rev bin))

let truthTable s =
  let e = s |> lexer |> parser |> fst in
  let vars = listVars e in
  let n = List.length vars in
  let rec helper currEnv res =
    if List.length currEnv = n + 1 then res
    else
      let env = List.combine vars currEnv in
      let v = eval env e in
      helper (addOne currEnv) ((env, v) :: res)
  in
  List.rev (helper (List.init n (fun _ -> false)) [])

let isSatisfiable s = List.exists snd (truthTable s)

let isTautology s = List.fold_left (fun res (_, b) -> b && res) true (truthTable s)

let print_truth_table tt =
  let print_entry (env, result) =
    let env_str =
      List.fold_left
        (fun acc (var, value) -> Printf.sprintf "%s%s=%b; " acc var value)
        "" env
    in
    Printf.printf "[%s] -> %b\n" env_str result
  in
  List.iter print_entry tt
