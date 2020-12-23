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

let explode input = input |> String.to_seq |> List.of_seq

let array_rest array index = 
  Array.sub array index (Array.length array - index)

let next_to_find first picked_out maximum =
  let attempt1 = if first = 1 then maximum else first - 1 in
  if Array.mem attempt1 picked_out then
      let attempt2 = if attempt1 = 1 then maximum else attempt1 - 1 in
    if Array.mem attempt2 picked_out then
      let attempt3 = if attempt2 = 1 then maximum else attempt2 - 1 in
      if Array.mem attempt3 picked_out then
        if attempt3 = 1 then maximum else attempt3 - 1
      else attempt3
    else attempt2
  else attempt1

let step_array_ll current rnext maximum = 
  let picked_up = [|!rnext.(current); !rnext.(!rnext.(current)); !rnext.(!rnext.(!rnext.(current)))|] in
  let next = next_to_find current picked_up maximum in
  !rnext.(current) <- !rnext.(picked_up.(2));
  !rnext.(picked_up.(2)) <- !rnext.(next);
  !rnext.(next) <- picked_up.(0);
  ()

let unfold_ll_array ll_arr starting =
  let rec r aux index = if ll_arr.(index) = starting then aux else
    r (ll_arr.(index) :: aux) ll_arr.(index)
  in r [] starting |> rev

let naloga1 string = 
  let data = explode string |> map (fun c -> (int_of_string @@ String.make 1 c)) in
  let next_arr = ref (Array.make 10 0) in
  List.iter2 (fun x y -> !next_arr.(x) <- y) data ((tl data) @ [hd data]);
  let current = ref @@ hd data in
  for i = 1 to 100 do
    step_array_ll !current next_arr 9;
    current := !next_arr.(!current);
  done;
  unfold_ll_array !next_arr 1
  |> fold_left (fun x y -> x ^ string_of_int y) ""

let naloga2 string =
  let data = explode string |> map (fun c -> (int_of_string @@ String.make 1 c)) |> Array.of_list in
  let true_data = (Array.append data (Array.init (1_000_000 - Array.length data) (fun i -> i + 10))) in
  let next_arr = ref (Array.make 1_000_001 0) in
  Array.iter2 (fun x y -> !next_arr.(x) <- y) true_data (Array.append (array_rest true_data 1) (Array.sub true_data 0 1));
  let current = ref @@ data.(0) in
  for i = 1 to 10_000_000 do
    step_array_ll !current next_arr 1_000_000;
    current := !next_arr.(!current);
  done;
  !next_arr.(1) * !next_arr.(!next_arr.(1))
  |> string_of_int

let day = "23"
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
