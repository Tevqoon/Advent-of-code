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
  let rec recursor aux op = function
    | [] -> aux
    | "+"::xs -> recursor aux (+) xs
    | "*"::xs -> recursor aux (fun x y -> x * y) xs
    |   x::xs -> recursor (op (int_of_string x) aux) op xs in
  recursor 0 (+) (String.split_on_char ' ' string)

let next_inner_paren string =
  let rec recurser level l r = function
    | ('(', i)::xs when level = 0 -> recurser 1 i r xs
    | ('(', _)::xs -> recurser (level + 1) l r xs
    | (')', i)::xs when level = 1 -> (l, i)
    | (')', _)::xs -> recurser (level - 1) l r xs
    | _::xs -> recurser level l r xs
    | [] -> failwith "yikeroo" in
  let l, r = recurser 0 0 0 (explode string |> mapi (fun i x -> (x, i))) in
  (String.sub string 0 l, 
   String.sub string (l + 1) (r - l - 1), 
   String.sub string (r + 1) (String.length string - r - 1)) 

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

let (++) str1 str2 =
  string_of_int ((int_of_string str1) + (int_of_string str2))

let parse_simple' string = 
  let symbols = String.split_on_char ' ' string in
  let pluses = symbols 
    |> mapi (fun i x -> (i, x))
    |> filter (fun (i, x) -> x = "+")
    |> map fst in
  let symbol_arr = Array.of_list symbols in
  List.iter (fun i -> 
    symbol_arr.(i + 1) <- symbol_arr.(i - 1) ++ symbol_arr.(i + 1);
    symbol_arr.(i - 1) <- "*";
    symbol_arr.(i)     <- "*";) pluses;
  Array.fold_left (fun x y -> if y = "*" then x else x * (int_of_string y)) 1 symbol_arr

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
