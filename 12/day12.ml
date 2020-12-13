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

let format string = 
  string_to_list string
  |> map (fun x -> (String.sub x 0 1, int_of_string (String.sub x 1 ((String.length x) - 1))))

let turner (x, y) (dir, v) =
  let angle = (if dir = "L" then 1.0 else -1.0) *. (Float.pi /. 2.0) *. (float_of_int (v / 90)) in
  let c, s = int_of_float (cos angle), int_of_float (sin angle) in
  (c * x - s * y, s * x + c * y)

let step (px, py) (dx, dy) instr = match instr with
  | "N", v -> (px, py + v), (dx, dy)
  | "S", v -> (px, py - v), (dx, dy)
  | "E", v -> (px + v, py), (dx, dy)
  | "W", v -> (px - v, py), (dx, dy)
  | "F", v -> (px + v * dx, py + v * dy), (dx, dy)
  | dir, v -> (px, py), turner (dx, dy) (dir, v)

let step' (px, py) (wx, wy) instr = match instr with
  | "N", v -> (px, py), (wx, wy + v)
  | "S", v -> (px, py), (wx, wy - v)
  | "E", v -> (px, py), (wx + v, wy)
  | "W", v -> (px, py), (wx - v, wy)
  | "F", v -> (px + v * wx, py + v * wy), (wx, wy)
  | dir, v -> (px, py), turner (wx, wy) (dir, v)

let rec looper stepper ((px, py), dir) = function
  | [] -> abs(px) + abs(py)
  | instr::rest -> looper stepper (stepper (px, py) dir instr) rest

let naloga1 string =
  format string
  |> looper step ((0, 0), (1, 0))
  |> string_of_int

let naloga2 string =
  format string
  |> looper step' ((0, 0), (10, 1))
  |> string_of_int

let day = "12"
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
