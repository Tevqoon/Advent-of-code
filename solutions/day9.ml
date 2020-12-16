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
  |> List.filter (fun s -> s <> "");;

let format string = 
  string_to_list string
  |> List.map int_of_string

let checker possible_sums number = 
  let rec r l1 l2 num = match l1, l2 with
    | x::xs, y::ys when x + y = num -> true
    | frst, y::ys -> r frst ys num
    | x::xs, [] -> r xs xs num
    | [], _ -> false in
  match possible_sums with
    | x::xs -> r (x::xs) xs number
    | [] -> failwith "y"

let rec first_k k lst = match lst with
  | [] -> failwith "not enough"
  | x::xs -> if k = 1 then [x] else x::first_k (k - 1) xs;;

let after_k k lst = 
  let rec r i = function
    | x::xs when i < k -> r (i + 1) xs
    | lst -> lst
  in r 0 lst

let naloga1 string = 
  let whole_list = format string in
  let first_25 = first_k 25 whole_list in
  let after_25 = after_k 25 whole_list in
  let rec r nums rest = match nums, rest with
    | x::xs, y::ys -> if checker nums y then r (xs @ [y]) ys else y
    | _ -> failwith "yike"
  in r first_25 after_25 |> string_of_int

let int_min lst = List.fold_left min (hd lst) (tl lst)
let int_max lst = List.fold_left max (hd lst) (tl lst)

let rec explodep target sum_list working_list = 
  let rec sumints i = function
    | [] -> i
    | x::xs -> sumints (i + x) xs in
  match sumints 0 sum_list, working_list with
    | x, y::ys when x < target -> explodep target (sum_list @ [y]) ys
    | x, _ when x = target -> int_min(sum_list) + int_max(sum_list)
    | x, _ when x > target -> 0
    | _ -> failwith "yy"

let naloga2 string nal1 = 
  let target = int_of_string nal1 in
  let whole_list = format string in
  let rec loop lst  = 
    match (explodep target [] lst), lst with
    | 0, x::xs -> loop xs
    | x, _ -> x
  in loop whole_list |> string_of_int

let day = "9"
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
  let part2 = naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("outputs/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
