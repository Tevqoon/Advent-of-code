#use "topfind"
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
  String.split_on_char '\n' string
  |> List.filter (fun s -> s <> "")

let explode input = input |> String.to_seq |> List.of_seq

let format string =
  Str.split (Str.regexp "\n\n") string
  |> map (String.split_on_char '\n')
  |> map tl
  |> map (map int_of_string)

let rec combat = function
  | [], lst -> ([], lst)
  | lst, [] -> (lst, [])
  | x::xs, y::ys when x > y      -> combat ((xs @ [x;y]), ys)
  | x::xs, y::ys (*when y > x *) -> combat (xs, (ys @ [y;x]))

let naloga1 string =
  let p1, p2 = match format string with a::b::[] -> (a, b) | _ -> failwith "smh" in
  match combat (p1, p2) with
  | [], lst | lst, [] ->
    mapi (fun i x -> (i + 1) * x) (rev lst) 
    |> fold_left (+) 0
    |> string_of_int
  | _ -> failwith "well yikerino, this ain't it"

let rec first_nth n lst = match n, lst with
  | 0, _ -> []
  | n, x::xs -> x :: (first_nth (n - 1) xs)
  | _ -> failwith "too short!"

let rec recombat states = function
  | [], lst -> ([], lst)
  | lst, [] -> (lst, [])
  | x1, x2 when mem (x1, x2) states -> (x1, [])
  | x::xs, y::ys when x <= length xs && y <= length ys -> 
    (match recombat [] (first_nth x xs, first_nth y ys) with
    | [], lst -> recombat ((x::xs, y::ys)::states) (xs, ys @ [y;x])
    | lst, [] -> recombat ((x::xs, y::ys)::states) (xs @ [x;y], ys)
    | _ -> failwith "ain't happ'nin'")
  | x::xs, y::ys when (x > length xs || y > length ys) && x > y -> recombat ((x::xs, y::ys)::states) (xs @ [x;y], ys)
  | x::xs, y::ys when (x > length xs || y > length ys) && x < y -> recombat ((x::xs, y::ys)::states) (xs, ys @ [y;x])
  | _ -> failwith "hmmm"

let naloga2 string = 
  let p1, p2 = match format string with a::b::[] -> (a, b) | _ -> failwith "smh" in
  match recombat [] (p1, p2) with
  | [], lst | lst, [] ->
    mapi (fun i x -> (i + 1) * x) (rev lst) 
    |> fold_left (+) 0
    |> string_of_int
  | _ -> failwith "well idk"

let day = "22"
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
