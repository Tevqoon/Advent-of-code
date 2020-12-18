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
  |> List.filter (fun s -> s <> "")

let explode input = input |> String.to_seq |> List.of_seq

let parse_simple string = 
  match string with
    | "" -> 0
    | _ ->
  let stuff = String.split_on_char ' ' string in
  let aux = ref (if string.[1] = '*' then 1 else 0)
  and op = ref (+) in
  List.iter (fun x -> match x with
    | "+" -> op := (+);
    | "*" -> op := (fun x y -> x * y);
    | n -> aux := (!op (int_of_string n) !aux);) stuff;
  !aux

let rest_of_string string index =
  String.sub string index (String.length string - index)

let next_inner_paren string =
  let lst = explode string |> mapi (fun i x -> (x, i)) in
  let l = ref 0
  and r = ref 0 in
  let level = ref 0 in
  let rec recurser lst = match lst, !level with
    | [], _ -> ()
    | (x, i)::xs, 0 when x = '(' -> l := i; level := 1; recurser xs
    | (x, i)::xs, _ when x = '(' -> level := !level + 1; recurser xs
    | (x, i)::xs, 1 when x = ')' -> r := i;
    | (x, i)::xs, _ when x = ')' -> level := !level - 1; recurser xs
    | (x, i)::xs, _ -> recurser xs
  in recurser lst;
  (String.sub string 0 !l, String.sub string (!l + 1) (!r - !l - 1), rest_of_string string (!r + 1)) 

let rec parse_paren parser string = 
  match String.index_opt string '(' with
    | None -> parser string
    | Some _ -> 
      let l, c, r = next_inner_paren string in 
        let a = parse_paren parser c in
      parse_paren parser (l ^ (string_of_int a) ^ r)

let naloga1 string = 
  string_to_list string
  |> map (parse_paren parse_simple)
  |> fold_left (+) 0
  |> string_of_int

let (+--+) str1 str2 =
  string_of_int ((int_of_string str1) + (int_of_string str2))

let parse_simple' string = 
  match string with
    | "" -> 0
    | _ ->
  let stuff = String.split_on_char ' ' string in
  let pluses = stuff 
    |> mapi (fun i x -> (i, x))
    |> filter (fun (i, x) -> x = "+")
    |> map fst in
  let stuff_array = Array.of_list stuff in
  List.iter (fun i -> 
    stuff_array.(i + 1) <- stuff_array.(i - 1) +--+ stuff_array.(i + 1);
    stuff_array.(i - 1) <- "remove";
    stuff_array.(i) <- "remove";) pluses;
  Array.to_list stuff_array
    |> filter (fun x -> x <> "remove" && x <> "*")
    |> map int_of_string
    |> fold_left (fun x y -> x * y) 1

let naloga2 string =
  string_to_list string
  |> map (parse_paren parse_simple')
  |> fold_left (+) 0
  |> string_of_int

let day = "18"
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
