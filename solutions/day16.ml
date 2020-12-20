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

let t_of_pair_list lst = 
  (hd lst, hd @@ tl lst)

let format_property line = 
  let l1 = String.split_on_char ':' line in
  let name = hd l1 in
  let l2 = String.split_on_char ' ' (String.trim @@ hd @@ tl l1) in
  let l3 = [hd l2; hd @@ tl @@ tl l2] in
  let nums = map (fun x -> String.split_on_char '-' x) l3 
    |> map (map int_of_string) in
  (name, t_of_pair_list (hd nums), t_of_pair_list (hd @@ tl nums))

let format string = 
  let lines = string
  |> String.split_on_char '\n' in
  let empty = lines
    |> mapi (fun i x -> (x, i))
    |> filter (fun (a, b) -> a = "")
    |> map snd in 
  let line_arr = Array.of_list lines in
  let my_ticket = String.split_on_char ',' line_arr.(hd empty + 2) |> map int_of_string in
  let rest_tickets = Array.sub line_arr (2 + (hd @@ tl empty)) (Array.length line_arr - (hd @@ tl empty) - 2) 
    |> Array.to_list |> map (String.split_on_char ',') |> map (map int_of_string) in
  let properties = Array.to_list @@ Array.sub line_arr 0 (hd empty)
    |> map format_property in
  (properties, my_ticket, rest_tickets)

let invalid_vals properties ticket = 
  let prop' = map (fun (x, y, z) -> (y, z)) properties
    |> map (fun (x, y) -> [x; y])
    |> flatten in
  ticket
    |> filter (fun x -> [] = filter (fun (y1, y2) -> y1 <= x && x <= y2) prop')

let naloga1 string =
  let properties, my_ticket, rest_tickets = format string in
  rest_tickets
  |> map (invalid_vals properties)
  |> flatten
  |> fold_left (+) 0
  |> string_of_int

(*https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists*)
let rec transpose list = match list with
| []             -> []
| []   :: xss    -> transpose xss
| (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let which_properties properties values =
  properties
  |> filter (fun (name, (l1, h1), (l2, h2)) -> values = filter (fun x -> (l1 <= x && x <= h1) || (l2 <= x && x <= h2)) values)
  |> map (fun (n, _, _) -> n)

let insert_into_empty to_insert to_insert_into =
  let rec r to_insert left todo = match todo, to_insert with
    | [], _ -> left
    | x::xs, y::ys when x = [] -> r ys (left @ [y]) xs
    | x::xs, rest -> r rest (left @ [x]) xs
  in r to_insert [] to_insert_into

let solve system =
  let rec r system =
    let singletons = filter (fun x -> 1 = length x) system |> map hd in
    if singletons = flatten system then singletons else
      map (filter (fun x -> not @@ mem x singletons)) system
      |> insert_into_empty (map (fun x -> [x]) singletons)
      |> r
  in r system

let is_departure string =
  length (String.split_on_char ' ' string) = 2
  && string.[0] = 'd'

let naloga2 string = 
  let properties, my_ticket, rest_tickets = format string in
  let valid = transpose @@ filter (fun x -> [] = invalid_vals properties x) rest_tickets in
  map (which_properties properties) valid
  |> solve
  |> mapi (fun i x -> (x, i))
  |> filter (fun (x, _) -> is_departure x)
  |> map (fun (_, i) -> nth my_ticket i)
  |> fold_left ( * ) 1
  |> string_of_int

let day = "16"
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
