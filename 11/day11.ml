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

let format string = 
  string_to_list string
  |> map explode
  |> map Array.of_list
  |> Array.of_list

let neighbors x y arr = 
  let my = Array.length arr - 1
  and mx = Array.length arr.(0) - 1 in
  if x < 0 || y < 0 then failwith "yike" else
  [(-1, -1);(0, -1);(1, -1);(-1,0);(1,0);(-1,1);(0,1);(1,1)]
  |> filter (fun (i, j) -> 0 <= x + i && x + i <= mx && 0 <= y + j && y + j <= my)
  |> map (fun (i, j) -> arr.(y + j).(x + i))
  |> fold_left (fun (x0, x1, x2) y -> match y with
                | 'L' -> (x0 + 1, x1, x2)
                | '#' -> (x0, x1 + 1, x2)
                | '.' -> (x0, x1, x2 + 1)
                | _ -> failwith "yike") (0, 0, 0) 

let update x y arr = 
  let cell = arr.(y).(x) in
  match cell, neighbors x y arr with
    | 'L', (_, 0, _) -> '#'
    | '#', (_, o, _) when o >= 4 -> 'L'
    | _ -> cell

let step arr = 
  let my = Array.length arr 
  and mx = Array.length arr.(0) in
  let new_arr = Array.make_matrix my mx ' ' in
  Array.iteri (fun j y -> Array.iteri (fun i _ -> new_arr.(j).(i) <- (update i j arr);) y) arr;
  new_arr

let small = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n"

let naloga1 string = 
  let data = format string in
  let rec loop prev = 
    let nstep = step prev in
    if nstep = prev then nstep else loop nstep
  in loop data
  |> Array.map (Array.fold_left (fun x y -> x + (if y = '#' then 1 else 0)) 0)
  |> Array.fold_left (+) 0
  |> string_of_int
  
let neighbors' x y arr =
  let my = Array.length arr - 1
  and mx = Array.length arr.(0) - 1 in
  if x < 0 || y < 0 then failwith "yike" else
  let possible = [(-1, -1);(0, -1);(1, -1);(-1,0);(1,0);(-1,1);(0,1);(1,1)]
  |> filter (fun (i, j) -> 0 <= x + i && x + i <= mx && 0 <= y + j && y + j <= my) in
  let rec finder (cx, cy) (i, j) =
    let (newx, newy) = (i + cx, j + cy) in
    if 0 > newx || newx > mx || 0 > newy || newy > my then '.' else
    if arr.(newy).(newx) = '.' then finder (newx, newy) (i, j)
    else arr.(newy).(newx) in
  map (finder (x, y)) possible
  |> fold_left (fun (x0, x1, x2) y -> match y with
              | 'L' -> (x0 + 1, x1, x2)
              | '#' -> (x0, x1 + 1, x2)
              | '.' -> (x0, x1, x2 + 1)
              | _ -> failwith "yike") (0, 0, 0) 

let update' x y arr = 
  let cell = arr.(y).(x) in
  match cell, neighbors' x y arr with
    | 'L', (_, 0, _) -> '#'
    | '#', (_, o, _) when o >= 5 -> 'L'
    | _ -> cell

let step' arr = 
  let my = Array.length arr 
  and mx = Array.length arr.(0) in
  let new_arr = Array.make_matrix my mx ' ' in
  Array.iteri (fun j y -> Array.iteri (fun i _ -> new_arr.(j).(i) <- (update' i j arr);) y) arr;
  new_arr

let small' = ".##.##.\n#.#.#.#\n##...##\n...L...\n##...##\n#.#.#.#\n.##.##.\n"
let small'' = ".......#.\n...#.....\n.#.......\n.........\n..#L....#\n....#....\n.........\n#........\n...#.....\n"

let naloga2 string =
  let data = format string in
  let rec loop prev = 
    let nstep = step' prev in
    if nstep = prev then nstep else loop nstep
  in loop data
  |> Array.map (Array.fold_left (fun x y -> x + (if y = '#' then 1 else 0)) 0)
  |> Array.fold_left (+) 0
  |> string_of_int

let day = "11"
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
