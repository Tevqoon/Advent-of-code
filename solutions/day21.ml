open List

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let lst_to_pair = function
  | a::b::[] -> (a, b)
  | _ -> failwith "incorrect input"

let format string =
  String.split_on_char '\n' string
  |> map (String.split_on_char '(')
  |> map lst_to_pair
  |> map (fun (a, b) -> String.split_on_char ' ' a, map String.trim (String.split_on_char ',' @@ String.sub b 9 (String.length b - 10)))
  |> map (fun (a, b) -> filter ((<>) "") a, filter ((<>) "") b)

let (/|) l1 l2 =
  filter (fun x -> mem x l1) l2

let (|/) l1 l2 =
  l1 @ l2
  |> fold_left (fun x y -> if mem y x then x else y :: x) []

let list_intersection = function
  | [] -> failwith "nondefined universe"
  | x::xs -> fold_left (/|) x xs

let get_all_with_property property pair_list =
  filter (fun (a, b) -> mem property b) pair_list

let get_equation pair_list property =
  get_all_with_property property pair_list
  |> map fst
  |> list_intersection

let insert_into_empty to_insert to_insert_into =
  let rec r to_insert left todo = match todo, to_insert with
    | [], _ -> left
    | x::xs, y::ys when x = [] -> r ys (left @ [y]) xs
    | x::xs, rest -> r rest (left @ [x]) xs
  in r to_insert [] to_insert_into

let solve system =
  let rec r system =
    let singletons = filter (fun x -> 1 = length x) system |> map hd in
    if singletons = flatten system then singletons else
      map (filter (fun x -> not @@ mem x singletons)) system
      |> insert_into_empty (map (fun x -> [x]) singletons)
      |> r
  in r system

let naloga1 string =
  let equations = format string in
  let allergens = fold_left (fun x (_, b) -> x |/ b) [] equations in
  let system = map (get_equation equations) allergens in
  let solved = List.combine allergens (solve system) in
  let allergenic = map snd solved in
  let ingredients = flatten @@ map fst equations in
  let non_allergenic = filter (fun x -> not @@ mem x allergenic) ingredients in
  length non_allergenic
  |> string_of_int

let naloga2 string = 
  let equations = format string in
  let allergens = fold_left (fun x (a, b) -> x |/ b) [] equations in
  let system = map (get_equation equations) allergens in
  let solved = List.combine allergens (solve system) in
  List.sort (fun (a1, _) (a2, _) -> String.compare a1 a2) solved
  |> map snd
  |> String.concat ","

let day = "21"
let input_data = preberi_datoteko ("inputs/day_" ^ day ^ ".in")
let main () =
  print_endline ("Solving DAY: " ^ day);
  
  let p1_start = Sys.time () in
  let part1 = naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  izpisi_datoteko ("outputs/day_" ^ day ^ "_1.out") part1;
  
  let p2_start = Sys.time () in
  let part2 = naloga2 input_data in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("outputs/day_" ^ day ^ "_2.out") part2;
  ()


let _ = main ()
