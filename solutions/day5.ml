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

let explode input = input |> String.to_seq |> List.of_seq

let counter char str = 
  List.fold_left (fun x y -> x + (if y = char then 1 else 0)) 0 (explode str)

let to_bin x = match x with
  | x when x = 'F' -> 0
  | x when x = 'B' -> 1
  | x when x = 'R' -> 1
  | x when x = 'L' -> 0
  | _ -> failwith "yikes"

let format string = 
  string |> string_to_list
  |> map explode
  |> map (map to_bin)

let int_exp base exp = 
  (float_of_int base) ** (float_of_int exp)
  |> int_of_float

let list_to_bin lst = 
  lst
  |> mapi (fun i x -> (int_exp 2 ((length lst) - i - 1)) * x)
  |> fold_left (+) 0

let naloga1 string = 
  format string
  |> map list_to_bin
  |> fold_left max 0
  |> string_of_int

let make_possibilities =
  let rec r lst n = match n with
  | 0 -> lst
  | x -> r (lst @ [x]) (n - 1)
  in r [] 935

let naloga2 string = 
  let ids = format string |> map list_to_bin in
  filter (fun x -> (not (mem x ids)) && (mem (x + 1) ids) && (mem (x - 1) ids)) make_possibilities
  |> hd |> string_of_int

let day = "5"
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


