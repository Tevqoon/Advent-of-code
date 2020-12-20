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
  |> List.filter (fun s -> s <> "")

let explode input = input |> String.to_seq |> List.of_seq

let t_of_lst lst = (hd lst, hd @@ tl lst)

let maskparser line =
  String.trim @@ hd @@ tl (string_to_list '=' line)

let bitstring_of_int num = 
  let rec r n str = match n with
    | 0 -> "0" ^ str
    | 1 -> "1" ^ str
    | x when x mod 2 = 0 -> r (x / 2) ("0" ^ str)
    | x -> r (x / 2) ("1" ^ str)
  in let n = r num "" in
  String.trim ((String.make (36 - String.length n) '0') ^ n)

let int_of_bitstring bstr = 
  let l = String.length bstr in
  explode bstr
  |> mapi (fun i x -> if x = '0' then 0 else int_of_float (2.0 ** (float_of_int (l - i - 1))))
  |> fold_left (+) 0

let memparser line = 
  let mem, vl = t_of_lst (string_to_list '=' line) in
  ("mem",
   int_of_string @@ String.sub mem 4 (String.length mem - 6),
   bitstring_of_int (int_of_string @@ String.trim vl))

let format string = 
  string_to_list '\n' string
  |> map (fun x -> match (String.get x 1) with
          | 'a' -> ("mask", 0, maskparser x)
          | 'e' -> memparser x
          | _ -> failwith "yikeroo")

let apply_mask mask value =
  value
  |> String.mapi (fun i c -> 
    match (String.get mask i) with
    | 'X' -> c
    | x -> x)

let naloga1 string = 
  let instr = format string in
  let memory = Hashtbl.create 10000 in
  let rec loop cmask = function
    | [] -> List.of_seq (Hashtbl.to_seq_values memory)
    | x::xs -> match x with
      | "mask", 0, nmask -> loop nmask xs
      | "mem", index, value -> Hashtbl.replace memory index (int_of_bitstring (apply_mask cmask value)); loop cmask xs
      | _ -> failwith "eggs benedict"
  in loop "" instr
  |> fold_left (+) 0
  |> string_of_int

let apply_mask' mask value = 
  value
  |> String.mapi (fun i c -> match (String.get mask i) with
    | '1' -> '1'
    | '0' -> c
    | x -> x)

let get_all_addresses str =
  let str_lst = explode str in
  let rec r strings = function
    | [] -> strings
    | x::xs -> match x with
      | '1' -> r (map (fun x -> x ^ "1") strings) xs
      | '0' -> r (map (fun x -> x ^ "0") strings) xs
      | _ -> r ((map (fun x -> x ^ "0") strings) @ (map (fun x -> x ^ "1") strings)) xs
  in r [""] str_lst
  |> map int_of_bitstring

let naloga2 string =
  let instr = format string in
  let memory = Hashtbl.create 100000 in 
  let rec loop cmask = function
     | [] -> List.of_seq (Hashtbl.to_seq_values memory)
     | x::xs -> match x with
      | "mask", 0, nmask -> loop nmask xs
      | "mem", index, value -> List.iter (fun x -> Hashtbl.replace memory x (int_of_bitstring value)) (get_all_addresses (apply_mask' cmask (bitstring_of_int index))); loop cmask xs
      | _ -> failwith "full english breakfast"
  in loop "" instr
  |> fold_left (+) 0
  |> string_of_int

let day = "14"
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
