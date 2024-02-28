open Parser
open Lexer
open Expression

type assignment = (string * bool) list
type truth_table = (assignment * bool) list

(** evaluates a given string as a formula in the empty environment *)
let code s = s |> lexer |> parser |> fst |> eval []

(** Compute the next assignment of variables.
    Counting up by one in binary representation.
*)
let increment_assignment bin =
  let rec helper bin =
    match bin with
    | [] -> []
    | false :: t -> true :: t (* negation *)
    | true :: t -> false :: helper t (* until first 0 *)
  in
  List.rev (helper (List.rev bin))

(** Computes a full truth table for an expression given as a string *)
let truth_table s =
  (* compute formula *)
  let e = s |> lexer |> parser |> fst in
  let vars = list_vars e in
  let n = List.length vars in
  (* go through all possible assignments and evaluate formula under it *)
  let rec helper currEnv res =
    (* compute result for current assignment *)
    let env = List.combine vars currEnv in
    let v = eval env e in
    let res' = (env, v) :: res in
    (* 1...1 is the last environment *)
    if List.for_all Fun.id currEnv then res'
    else helper (increment_assignment currEnv) res'
  in
  let init_env = List.init n (fun _ -> false) in
  helper init_env [] |> List.rev

let is_satisfiable s = List.exists snd (truth_table s)
let is_tautology s = List.for_all snd (truth_table s)

let print_truth_table (tt : truth_table) =
  let show_row (assignment, result) =
    "[" ^ String.concat "; " (
      (* var or !var if false *)
      List.map (fun (var, value) -> (if value then " " else "!")^var) assignment
    ) ^ "] -> " ^ string_of_bool result
  in
  String.concat "\n" (List.map show_row tt)
  |> print_endline