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

let string_to_list char string = 
  string |> String.split_on_char char |> List.filter (fun s -> s <> "");;

let format string =
  string |> string_to_list '\n' |> map (string_to_list ' ')
  |> map (fun lst -> (hd lst, int_of_string (hd (tl lst))))
  |> Array.of_list

module IS = Set.Make(Int);;

let rec stepper array vis ind acc = 
  let (instr, next) = Array.get array ind
  and state = ind >= (Array.length array) - 1 in
  if IS.mem ind vis || state then (state, acc) else match instr with
  | "acc" -> stepper array (IS.add ind vis) (ind + 1) (acc + next)
  | "nop" -> stepper array (IS.add ind vis) (ind + 1) acc
  | "jmp" -> stepper array (IS.add ind vis) (ind + next) acc
  | _ -> failwith "yike"

let naloga1 string = 
  let prog = format string in
  stepper prog IS.empty 0 0 |> (fun (x, y) -> y) |> string_of_int

let flip = function
  | ("nop", y) -> ("jmp", y)
  | ("jmp", y) -> ("nop", y)
  | _ -> failwith "yike"

let naloga2 string =
  let prog = format string in
  let rec loop i =
    let new_prog = Array.copy prog in
    match Array.get prog i with
      | ("acc", y) -> loop (i + 1)
      | (x, y) -> Array.set new_prog i (flip (x, y));
    match stepper new_prog IS.empty 0 0 with
      | (true, acc) -> acc
      | (false, _) -> loop (i + 1)
  in loop 0 |> string_of_int

let day = "8"
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