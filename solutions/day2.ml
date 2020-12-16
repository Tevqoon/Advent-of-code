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

(*
6-7 z: dqzzzjbzz
*)

let num_splitter numstr =
    match String.split_on_char '-' numstr with
    | n1::n2::[] -> (int_of_string n1, int_of_string n2)
    | _ -> failwith "yikes bro" 

let formatter line =
  let lst = String.split_on_char ' ' line in
  match lst with
  | nums::char::pass::[] -> 
    let t1, t2 = num_splitter nums in
      (t1, t2, String.get char 0, pass)
  | _ -> failwith "yikes bro"

let explode input = input |> String.to_seq |> List.of_seq

let counter char str = 
  List.fold_left (fun x y -> x + (if y = char then 1 else 0)) 0 (explode str)

let checker (n1, n2, char, str) = 
  let c = counter char str in
  c >= n1 && c <= n2

let checker' (n1, n2, char, str) = match (str.[n1 - 1], str.[n2 - 1]) with
  | c, c' when c = char && c' = char  -> false
  | c, c' when c = char || c' = char  -> true
  | _ -> false 

let naloga1 string =
  let lines = List.map formatter (string_to_list string) in
  List.fold_left (fun x y -> x + (if (checker y) then 1 else 0)) 0 lines
  |> string_of_int

let naloga2 string = 
  let lines = List.map formatter (string_to_list string) in
  List.fold_left (fun x y -> x + (if (checker' y) then 1 else 0)) 0 lines
  |> string_of_int

let main () =
  let day = "2" in
  print_endline ("Solving DAY: " ^ day);
  let input_data = preberi_datoteko ("inputs/day_" ^ day ^ ".in") in

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
