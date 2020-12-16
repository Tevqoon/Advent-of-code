#require "str"
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

let string_to_list string = 
  string
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "")

let string_to_list' char string = 
  string
  |> String.split_on_char char
  |> List.filter (fun s -> s <> "")
  |> List.map String.trim

let explode input = input |> String.to_seq |> List.of_seq

let counter char str = 
  List.fold_left (fun x y -> x + (if y = char then 1 else 0)) 0 (explode str)

let parse_bag bag = 
  let num = int_of_string (String.make 1 (String.get bag 0))
  and name = String.sub bag 2 (String.length bag - 2)
  |> string_to_list' ' '
  |> (fun lst -> (hd lst) ^ " " ^ (hd (tl lst)))
  in (name, num)

let parse_rest bags = 
  if bags = "no other bags." then [] else
  bags |> string_to_list' ','
  |> map parse_bag

let parse_line line =
  Str.split (Str.regexp " bags contain ") line
  |> (fun lst -> (hd lst, parse_rest (hd (tl lst))))

let format string =
  string_to_list string
  |> map parse_line

(* ("dotted salmon", [("dark lavender", 2); ("muted red", 1); ("vibrant magenta", 1)])*)
let flatten_bags (name, rest_lst) = 
    let rest_names = List.map (fun (x, y) -> x) rest_lst in
    (name, rest_names)

let sample = [("dotted salmon", [("dark lavender", 2); ("muted red", 1); ("vibrant magenta", 1)]);
 ("vibrant purple", [("pale cyan", 1); ("dotted lavender", 1); ("striped blue", 3); ("clear magenta", 5)])]

let find_bags_that_contain bag bags = 
  let flattened = map flatten_bags bags in
  filter (fun (x, lst) -> mem bag lst) flattened
  |> map (fun (x, lst) -> x)

(*iz https://rosettacode.org/wiki/Remove_duplicate_elements#OCaml*)
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let naloga1 string =
  let all_bags = format string 
  and bag = "shiny gold" in
  let rec r todo found = match todo with
    | [] -> found
    | x::xs -> let f = find_bags_that_contain x all_bags in r (xs @ f) (found @ f)
  in r [bag] [] |> uniq |> length |> string_of_int

let assoc bag_name bags =
  filter (fun (x, y) -> x = bag_name) bags |> hd
let bags_of (x, y) = y

let naloga2 string = 
  let all_bags = format string in
  let bag = assoc "shiny gold" all_bags in
  let rec r bg = 
    fold_left (+) 0 (map (fun (x, y) -> y + y * r(assoc x all_bags)) (bags_of bg))
  in r bag |> string_of_int

let day = "7"
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



