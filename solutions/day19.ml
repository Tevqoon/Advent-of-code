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

let rule_parser line = 
  let s1 = String.split_on_char ':' line in
  let num = int_of_string @@ hd s1 in
  let rest = String.split_on_char '|' (hd @@ tl s1)
    |> rev_map (String.split_on_char ' ')
    |> rev_map (filter (fun x -> x <> "")) in
  match int_of_string_opt (hd @@ hd rest) with
    | None   -> (num, [[String.sub (hd @@ hd rest) 1 1]])
    | Some _ -> (num, rest)

let format string = 
  let rec splitter aux to_match = function
    | [] -> (aux, [])
    | x::xs when x = to_match -> (aux, xs)
    | x::xs -> splitter (aux @ [x]) to_match xs in 
  let rules, strings = String.split_on_char '\n' string
    |> splitter [] ""
    |> fun (x, y) -> (map rule_parser x, y) in
  let retarray = Array.make (length rules) [[]] in
  List.iter (fun (i, x) -> retarray.(i) <- x;) rules;
  (retarray, strings)

(*https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists*)
let rec cartesian l = 
  let rec aux ~acc l1 l2 = match l1, l2 with
  | [], _ | _, [] -> acc
  | h1::t1, h2::t2 -> 
      let acc = (h1::h2)::acc in
      let acc = (aux ~acc t1 l2) in
      aux ~acc [h1] t2
  in match l with
  | [] -> []
  | [l1] -> List.map (fun x -> [x]) l1
  | l1::tl ->
      let tail_product = cartesian tl in
      aux ~acc:[] l1 tail_product

let concat_strings l = map (String.concat "") (cartesian l)
let add_if_not_in lst el = if mem el lst then lst else lst @ [el]

let rec string_generator rule_array index =
  let c_rule = rule_array.(index) in
  match (hd @@ hd c_rule) with
    | "a" -> ["a"]
    | "b" -> ["b"]
    |   _ -> 
  map (map int_of_string) c_rule
    |> map (fun path ->
       concat_strings (map (string_generator rule_array) path))
    |> flatten
    |> fold_left add_if_not_in []

let slice_eights string =
  List.init (String.length string / 8) (fun i -> String.sub string (i * 8) 8)

let checker lists string =
  let slices = slice_eights string in
  length slices = length lists
  && for_all2 mem slices lists

let naloga1 string = 
  let rules, strings = format string in
  let f42, t31 = string_generator rules 42, string_generator rules 31 in
  strings
  |> filter (checker [f42;f42;t31])
  |> length
  |> string_of_int

let n_list n x = List.init n (fun _ -> x)

let naloga2 string =
  let rules, strings = format string in
  let f42, t31 = string_generator rules 42, string_generator rules 31
  and found = ref [] in
  for k = 2 to 9 do
    for i = 1 to k - 1 do
      let checklist = (n_list k f42) @ (n_list i t31) in
      let additions = filter (checker checklist) strings in
      found := !found @ additions;
    done;
  done; !found |> length |> string_of_int

let day = "19"
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
