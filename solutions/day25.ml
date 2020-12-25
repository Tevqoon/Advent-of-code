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
  String.split_on_char '\n' string 
  |> map int_of_string
  |> fun lst -> (hd lst, hd @@ tl lst)

let transform subject loop_size = 
  let n = ref 1 in
  for i = 1 to loop_size do
    n := (!n * subject) mod 20201227;
  done; !n

let find_loop_size key subject =
  let rec recursor loop_size value =
    if value = key then loop_size 
    else 
      recursor (succ loop_size) (value * subject mod 20201227) in
    recursor 0 1

let naloga1 string = 
  let k1, k2 = format string in
  transform k1 (find_loop_size k2 7)
  |> string_of_int

let day = "25"
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
  ()

let _ = main ()
