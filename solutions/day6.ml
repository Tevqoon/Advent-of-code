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
  (*|> List.filter (fun s -> s <> "");;*)
let explode input = input |> String.to_seq |> List.of_seq

let format string =
  let rec r finished line lines = match lines with
    | [] -> finished
    | head::blank::rest when blank = "" -> r (finished @ [line ^ head]) "" rest
    | head::rest -> r finished (line ^ head ^ " ") rest
  in
  string
  |> string_to_list
  |> r [] ""
  |> List.map explode

let counter char char_lst = 
  List.fold_left (fun x y -> x + (if y = char then 1 else 0)) 0 char_lst

let question_counter char_lst = 
  let rec r counts chars = match chars with
    | [] -> counts 
    | x::xs -> r (counts @ [(x, counter x chars)])
                 (filter (fun y -> y <> x) xs)
  in r [] char_lst

let naloga1 string =
  format string
  |> List.map (List.filter (fun x -> x <> ' '))
  |> map question_counter
  |> map List.length 
  |> List.fold_left (+) 0
  |> string_of_int

let naloga2 string = 
  let char_lists = format string in
  let people_counts = char_lists |> map (fun x -> (counter ' ' x) + 1) in
  char_lists
  |> List.map (List.filter (fun x -> x <> ' '))
  |> map question_counter
  |> map2 (fun x y -> filter (fun (a, c) -> c = x) y) people_counts
  |> map length
  |> fold_left (+) 0
  |> string_of_int

let day = "6"
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
