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

let format str =
  str
  |> string_to_list
  |> List.map explode

let descend1 tobolist slopex = 
  let width = List.length (List.nth tobolist 0) in
  let rec r tobo current_x trees = match tobo with
    | [] -> trees
    | line::rest -> 
      let field = List.nth line current_x in
      let tree = (if field = '#' then 1 else 0) in
      r rest ((current_x + slopex) mod width) (trees + tree)
  in r tobolist 0 0

let descendbyn tobolist slopex slopey = 
  let width = List.length (List.nth tobolist 0) in
  let rec r tobo current_x_mod current_y_mod trees = match tobo with
  | [] -> trees
  | line::rest when current_y_mod != 0 -> r rest current_x_mod ((current_y_mod + 1) mod slopey) trees
  | line::rest when current_y_mod = 0-> 
      let field = List.nth line current_x_mod in
      let tree = (if field = '#' then 1 else 0) in
      r rest ((current_x_mod + slopex) mod width) 1 (trees + tree)
  | _ -> failwith "How did we get here?"
  in r tobolist 0 0 0

let naloga1 string = 
  let trees = format string in
  descend1 trees 3
  |> string_of_int

let naloga2 string = 
  let trees = format string in
  let d1 = descend1 trees 1
  and d2 = descend1 trees 3
  and d3 = descend1 trees 5
  and d4 = descend1 trees 7
  and d5 = descendbyn trees 1 2 in
  d1 * d2 * d3 * d4 * d5
  |> string_of_int

let main () =
  let day = "3" in
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
