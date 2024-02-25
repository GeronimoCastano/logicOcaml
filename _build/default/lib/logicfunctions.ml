open Parser
open Lexer
open Expression
open Utils

let code s = 
  s |> lexer |> parser |> fst |> (eval []);;



let truthTable s = 
  let e = s |> lexer |> parser |> fst in
  let vars = listVars e in
  let n = List.length vars in
  let rec helper currEnv res = 
    if List.length currEnv = n + 1 then res
    else let env = zip vars currEnv in
    let v = eval env e in 
    helper (addOne currEnv) ((env, v) :: res)
  in List.rev (helper (List.init n (fun _ -> false)) []);;


let isSatisfiable s = 
  List.exists (fun (_, b) -> b) (truthTable s);;



let isTautology s = 
  foldl (fun (_ , b) res -> if b then res else false) (truthTable s) true;;



let rec print_truth_table tt =
  let print_entry (env, result) =
    let env_str = List.fold_left (fun acc (var, value) ->
      Printf.sprintf "%s%s=%b; " acc var value) "" env in
    Printf.printf "[%s] -> %b\n" env_str result
  in
  List.iter print_entry tt;;

