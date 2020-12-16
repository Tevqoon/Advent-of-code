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

let format string =
  string
  |> String.split_on_char ','
  |> map int_of_string
  |> rev

let looper' num data =
  let things = Array.make num 0 in
  iteri (fun i x -> things.(x) <- i + 1) (rev (tl data));
  let prev = ref (hd data) in
  for turn = (1 + length data) to num do
  match things.(!prev) with
      | 0 -> things.(!prev) <- turn - 1; prev := 0;
      | y -> things.(!prev) <- turn - 1; prev := turn - y - 1;
  done; !prev

let naloga1 string = 
  string |> looper' 2020 |> string_of_int

let naloga2 string = 
  string |> looper' 30_000_000 |> string_of_int

let day = "15"
let input_data = preberi_datoteko ("inputs/day_" ^ day ^ ".in") |> format
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
