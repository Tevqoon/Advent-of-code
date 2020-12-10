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
  |> filter (fun s -> s <> "");;

let format string =
  string_to_list string
  |> map int_of_string
  |> sort compare

let naloga1 string =
  let data = format string in
  let l1 = 0 :: data in
  let l2 = data @ [(hd (rev data)) + 3] in
  map2 (fun x y -> y - x) l1 l2
  |> (fun x -> (length (filter ((=) 1) x))
             * (length (filter ((=) 3) x)))
  |> string_of_int

let naloga2 string = 
  let data = rev (0::(format string)) in
  let head = (hd data) + 3 in
  let data = rev (head :: data) in
  let paths = Array.make (head + 1) 0 in
  Array.iteri (fun i _ -> if mem i data then 
  Array.set paths i (match i with
     | 0 -> 1
     | 1 -> 1
     | 2 -> (paths.(0) + paths.(1))
     | n -> (paths.(i - 1) + paths.(i - 2) + paths.(i - 3)));) paths;
  paths.(head) |> string_of_int

let day = "10"
let input_data = preberi_datoteko (day ^ "/day_" ^ day ^ ".in")
let main () =
  print_endline ("Solving DAY: " ^ day);

  let p1_start = Sys.time () in
  let part1 = naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  izpisi_datoteko (day ^ "/day_" ^ day ^ "_1.out") part1;
  
  let p2_start = Sys.time () in
  let part2 = naloga2 input_data in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko (day ^ "/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()