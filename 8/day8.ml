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
  string
  |> String.split_on_char char
  |> List.filter (fun s -> s <> "");;

let format string =
  string |> string_to_list '\n'
  |> map (string_to_list ' ')
  |> map (fun lst -> (hd lst, int_of_string (hd (tl lst))))
  |> Array.of_list

let naloga1 string = 
  let prog = format string in
  let rec r visited i acc = 
    let (instr, next) = Array.get prog i in
    if mem i visited then acc else match instr with
    | "acc" -> r (visited @ [i]) (i + 1) (acc + next)
    | "nop" -> r (visited @ [i]) (i + 1) acc
    | "jmp" -> r (visited @ [i]) (i + next) acc
    | _ -> failwith "yike"
  in r [] 0 0 |> string_of_int

(*May god forgive me for what I am about to do.*)
(*This is gonna be the jankiest, ugliest code of this decade. Befitting 2020, so to speak *)
let naloga2 string = 
  let prog = format string in
  let len = (Array.length prog) - 1 in
  let rec loop i =
    if i >= len then failwith "yike?" else
    let (x, y) = Array.get prog i in
    if x = "acc" then loop (i + 1) else
    let new_op = match x with
      | "nop" -> ("jmp", y)
      | "jmp" -> ("nop", y) 
      | _ -> failwith "yike!" in
    let new_prog = Array.copy prog in
      Array.set new_prog i new_op;
    let rec runner visited index acc =
      let (instr, next) = Array.get new_prog index in
      if mem index visited then (loop (i + 1))
      else if index >= len then acc else match instr with
        | "acc" -> runner (visited @ [index]) (index + 1) (acc + next)
        | "nop" -> runner (visited @ [index]) (index + 1) acc
        | "jmp" -> runner (visited @ [index]) (index + next) acc
        | _ -> failwith "yike"
      in runner [] 0 0 in loop 0 |> string_of_int

let day = "8"
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
