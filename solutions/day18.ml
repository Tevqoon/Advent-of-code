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

let parse_simple string = 
  let rec recursor aux op = function
    | [] -> aux
    | "+"::xs -> recursor aux (+) xs
    | "*"::xs -> recursor aux ( * ) xs
    |   x::xs -> recursor (op (int_of_string x) aux) op xs in
  recursor 0 (+) (String.split_on_char ' ' string)

let (++) str1 str2 = string_of_int(int_of_string str1 + int_of_string str2)

let parse_simple2 string = 
  let symbols = Array.of_list (String.split_on_char ' ' string) in
  Array.iteri (fun i x -> if x = "+" then (
    symbols.(i + 1) <- symbols.(i - 1) ++ symbols.(i + 1);
    symbols.(i - 1) <- "*";
    symbols.(i)     <- "*";)) symbols;
  Array.fold_left (fun x y -> if y = "*" then x else x * (int_of_string y)) 1 symbols

let next_inner_paren string =
  let rec recurser level i l lst = match lst with
    | ')'::xs when level = 1 -> (l, i)
    | '('::xs -> recurser (level + 1) (i + 1) (if level = 0 then i else l) xs
    | ')'::xs -> recurser (level - 1) (i + 1) l xs
    |   _::xs -> recurser level (i + 1) l xs
    | [] -> failwith "yikeroo" in
  let l, r = recurser 0 0 0 (List.of_seq @@ String.to_seq string) in
   (String.sub string 0 l,
    String.sub string (l + 1) (r - l - 1), 
    String.sub string (r + 1) (String.length string - r - 1))

let rec parse_paren parser string = 
  match String.index_opt string '(' with
    | None   -> parser string
    | Some _ -> let l, c, r = next_inner_paren string in 
      parse_paren parser (l ^ (string_of_int @@ parse_paren parser c) ^ r)

let naloga1 string = 
  String.split_on_char '\n' string
  |> rev_map (parse_paren parse_simple) |> fold_left (+) 0 |> string_of_int
let naloga2 string =
  String.split_on_char '\n' string
  |> rev_map (parse_paren parse_simple2) |> fold_left (+) 0 |> string_of_int

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
