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

module PS = Set.Make(struct type t = int * int let compare = compare end)
let (+.+) (a, b) (c, d) = (a + c, b + d)

let format string =
  let rec recursor aux mode = function
    | [] -> aux
    | 'e'::xs when mode = 'n' -> recursor ((1,1) +.+ aux) ' ' xs
    | 'w'::xs when mode = 'n' -> recursor ((-1,1) +.+ aux) ' ' xs
    | 'e'::xs when mode = 's' -> recursor ((1,-1) +.+ aux) ' ' xs
    | 'w'::xs when mode = 's' -> recursor ((-1,-1) +.+ aux) ' ' xs
    | 'e'::xs -> recursor ((2,0) +.+ aux) ' ' xs
    | 'w'::xs -> recursor ((-2,0) +.+ aux) ' ' xs
    | 'n'::xs -> recursor aux 'n' xs
    | 's'::xs -> recursor aux 's' xs
    | _ -> failwith "yikra"
  in 
  String.split_on_char '\n' string
  |> map (fun line -> recursor (0, 0) ' ' (line |> String.to_seq |> List.of_seq))

let potential_neighbors point = map (fun x -> x +.+ point) [(1,1);(-1,1);(1,-1);(-1,-1);(2,0);(-2,0)]

let active_neighbor_num point set =
  length @@ filter (fun x -> PS.mem x set) (potential_neighbors point)

let update set point = match (PS.mem point set), (active_neighbor_num point set) with
  | true, 1 | true, 2 | false, 2 -> true
  | _ -> false

let step set =
  let to_check = PS.of_list @@ flatten @@ map potential_neighbors (PS.elements set) in
  PS.filter (update set) to_check

let rec app_n n f x = match n with
  | 0 -> x
  | n -> f (app_n (pred n) f x)

let naloga stepn formatted = 
  let tiles = PS.filter (fun x -> (length @@ filter ((=) x) formatted) mod 2 <> 0) (PS.of_list formatted) in
  app_n stepn step tiles |> PS.cardinal |> string_of_int

let day = "24"
let input_data = preberi_datoteko ("inputs/day_" ^ day ^ ".in") |> format
let main () =
  print_endline ("Solving DAY: " ^ day);
  
  let p1_start = Sys.time () in
  let part1 = naloga 0 input_data in
  let t1_time = Sys.time () -. p1_start in
  
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  izpisi_datoteko ("outputs/day_" ^ day ^ "_1.out") part1;
  
  let p2_start = Sys.time () in
  let part2 = naloga 100 input_data in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("outputs/day_" ^ day ^ "_2.out") part2;
  ()


let _ = main ()
