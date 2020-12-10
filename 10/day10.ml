open List
#use "topfind"
#require "zarith"

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
  |> filter (fun s -> s <> "")

let format string =
  let arr = string_to_list string 
  |> map int_of_string
  |> Array.of_list in
  Array.sort compare arr; arr
let get_last array =
  Array.get array ((Array.length array) - 1)

let naloga1 formatted =
  let data = formatted in
  let a1 = Array.append (Array.make 1 0) data in
  let a2 = Array.append data (Array.make 1 ((get_last data) + 3)) in
  Array.map2 (fun x y -> y - x) a1 a2
  |> Array.fold_left (fun (x1, x3) y -> match y with
                      | 1 -> (x1 + 1, x3) 
                      | 3 -> (x1, x3 + 1)
                      | _ -> (x1, x3)) (0, 0)
  |> (fun (x, y) -> x * y) |> string_of_int

let naloga2 formatted = 
  let data = formatted in
  let head = 3 + get_last data in
  let data =  Array.concat [(Array.make 1 0); data; (Array.make 1 head)] in
  let paths = Array.make (head + 1) Z.zero in
  Array.iter (fun i -> Array.set paths i (match i with
      | 0 -> Z.one
      | 1 -> Z.one
      | 2 -> Z.add paths.(0) paths.(1)
      | n -> Z.add paths.(i - 1) (Z.add paths.(i - 2) paths.(i - 3)));) data;
  paths.(head) |> Z.to_string

let day = "10"
let input_data = preberi_datoteko (day ^ "/day_" ^ day ^ ".in") |> format
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

let big_input_data = preberi_datoteko (day ^ "/bigboy.in") |> format
let bigboy () =
  print_endline ("Solving BIGBOY: " ^ day);
  print_endline "BIGBOY 1:";
  let p1_start = Sys.time () in
  let part1 = naloga1 big_input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  izpisi_datoteko (day ^ "/day_" ^ day ^ "_big_1.out") part1;
  
  print_endline "BIGBOY 2:";
  let p2_start = Sys.time () in
  let part2 = naloga2 big_input_data in
  let t2_time = Sys.time () -. p2_start in
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko (day ^ "/day_" ^ day ^ "_big_2.out") part2;
  ()