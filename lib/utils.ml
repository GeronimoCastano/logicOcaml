
let rec lookup x env = 
  match env with
  | [] -> failwith "Not found"
  | (k, v) :: t -> if x = k then v else lookup x t;;


let rec foldl f list init = 
  match list with
  | [] -> init
  | h :: t -> foldl f t (f h init);;


let addOne bin = 
  let rec helper bin =
  match bin with
  | [] -> [true]
  | h :: t -> if h then false :: helper t else true :: t
  in List.rev (helper (List.rev bin));;



let rec zip list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2
  | _ -> failwith "Not same size";;

