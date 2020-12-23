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

let naloga1 string = 
  let nums = List.map int_of_string (string_to_list string) in
  let rec r l1 l2 = match l1, l2 with
    | x::xs, y::ys when x + y = 2020 -> string_of_int (x * y)
    | frst, y::ys -> r frst ys
    | x::xs, [] -> r xs xs
    | [], _ -> "None"
  in r nums nums

let naloga2 string = 
  let nums = List.map int_of_string (string_to_list string) in
  let rec r l1 l2 l3 = match l1, l2, l3 with
    | x::xs, y::ys, z::zs when x + y + z = 2020 -> string_of_int (x * y * z)
    | x, y, z::zs -> r x y zs
    | x, y::ys, [] -> r x ys ys
    | x::xs, [], _ ->  r xs xs xs
    | [], _, _ -> "None"
  in r nums nums nums

let main () =
  let day = "1" in
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
