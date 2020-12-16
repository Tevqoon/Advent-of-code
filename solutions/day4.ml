open String
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
  (*|> List.filter (fun s -> s <> "")*)

let explode input = input |> String.to_seq |> List.of_seq

let counter char str = 
  List.fold_left (fun x y -> x + (if y = char then 1 else 0)) 0 (explode str)

let format string =
  let rec r finished line lines = match lines with
    | [] -> finished
    | head::blank::rest when blank = "" -> r (finished @ [line ^ head]) "" rest
    | head::rest -> r finished (line ^ head ^ " ") rest
  in
  string
  |> string_to_list
  |> r [] ""

let list_to_first = function
  | a::b::[] -> a
  | _ -> failwith "yikes"

let list_to_pair = function
  | a::b::[] -> (a, b)
  | _ -> failwith "yikes"

let make_pair_fst str = 
  str
  |> split_on_char ':'
  |> list_to_first

let make_pairs str =
  str
  |> split_on_char ':'
  |> list_to_pair

(*["ecl"; "byr"; "iyr"; "pid"; "cid"; "hgt"; "eyr"; "hcl"]*)
let parse line = 
  line
  |> split_on_char ' '
  |> map (fun x -> make_pair_fst x)
  |> filter (fun x -> x <> "cid")

let naloga1 string = 
  format string
  |> map parse
  |> map length
  |> fold_left (fun x y -> x + (if y >= 7 then 1 else 0)) 0
  |> string_of_int

let parse' line = 
  line
  |> split_on_char ' '
  |> map (fun x -> make_pairs x)
  |> filter (fun (x, y) -> x <> "cid")

let check_str s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false

let height_check str =
  if String.length str = 4 then
    ((sub str 2 2) = "in") && (int_of_string (sub str 0 2) >= 59) && (int_of_string (sub str 0 2) <= 76)
  else if String.length str = 5 then
    ((sub str 3 2) = "cm") && (int_of_string (sub str 0 3) >= 150) && (int_of_string (sub str 0 3) <= 193)
  else false

let pair_checker = function
  | a, b when a = "byr" -> 1920 <= (int_of_string b) && (int_of_string b) <= 2002
  | a, b when a = "iyr" -> 2010 <= (int_of_string b) && (int_of_string b) <= 2020
  | a, b when a = "eyr" -> 2020 <= (int_of_string b) && (int_of_string b) <= 2030
  | a, b when a = "hgt" -> height_check b
  | a, b when a = "hcl" -> b.[0] = '#'
  | a, b when a = "ecl" -> mem b ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
  | a, b when a = "pid" -> (String.length b) = 9 && check_str b
  | _ -> failwith "yikes"

let check' line =
  if length line >= 7 then
    fold_left (fun x y -> x && (pair_checker y)) true line
  else false

let naloga2 string = 
  format string
  |> map parse'
  |> map check'
  |> fold_left (fun x y -> x + if y then 1 else 0) 0
  |> string_of_int

let day = "4"
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
