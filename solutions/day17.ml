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
  |> Array.make 1

let diagonal3 x = (x, x, x)
let cartesian3 (l1, l2, l3) = flatten (flatten (map (fun x -> map (fun y -> map (fun z -> x, y, z) l3) l2) l1))

let neighbors x y z arr = 
  let mz = Array.length arr - 1
  and my = Array.length arr.(0) - 1
  and mx = Array.length arr.(0).(0) - 1 in
  cartesian3 (diagonal3 [-1;0;1])
  |> filter (fun (i, j, k) -> (i, j, k) <> (0, 0, 0) && 0 <= x + i && x + i <= mx && 0 <= y + j && y + j <= my && 0 <= z + k && z + k <= mz)
  |> map (fun (i,j,k) -> arr.(z + k).(y + j).(x + i))
  |> fold_left (fun active y -> match y with
                | '#' -> active + 1
                | _ -> active) 0 

let safe_get default z y x arr =
  try arr.(z).(y).(x) with Invalid_argument _ -> default

let update x y z arr = 
  let cell = safe_get '.' z y x arr in
  match cell, neighbors x y z arr with
    | '.', 3 -> '#'
    | '#', 2 | '#', 3 -> '#'
    | _ -> '.'

let next_size arr = 
  let mz = Array.length arr + 2
  and my = Array.length arr.(0) + 2
  and mx = Array.length arr.(0).(0) + 2 in
  let new_arr = Array.init mz (fun _ -> (Array.make_matrix my mx '.')) in
  Array.iteri (fun k z ->
    Array.iteri (fun j y ->
      Array.iteri (fun i x -> 
        new_arr.(k).(j).(i) <- safe_get '.' (k - 1) (j - 1) (i - 1) arr
      ) y) z) new_arr;
  new_arr

let mapi3d f arr = 
  let mz = Array.length arr
  and my = Array.length arr.(0)
  and mx = Array.length arr.(0).(0) in
  let new_arr = Array.init mz (fun _ -> (Array.make_matrix my mx (f 0 0 0 arr))) in
  Array.iteri (fun k z ->
    Array.iteri (fun j y ->
      Array.iteri (fun i _ -> 
        new_arr.(k).(j).(i) <- (f i j k arr)
      ) y) z) arr;
  new_arr

let step arr = 
  mapi3d update (next_size arr)

let count_active arr = 
  let z = Array.length arr - 1
  and y = Array.length arr.(0) - 1
  and x = Array.length arr.(0).(0) - 1 in
  let count = ref 0 in
  for k = 0 to z do
    for j = 0 to y do
      for i = 0 to x do
        if arr.(k).(j).(i) = '#' then count := !count + 1
  done; done; done; !count

let naloga1 string = 
  let current = ref (format string) in
  for i = 1 to 6 do
    current := step !current
  done; !current
  |> count_active
  |> string_of_int

let diagonal4 x = (x, x, x, x)
let cartesian4 (l1, l2, l3, l4) = flatten(flatten (flatten (map (fun x -> map (fun y -> map (fun z -> map (fun w -> x, y, z, w) l4) l3) l2) l1)))

let neighbors4 x y z w arr = 
  let mw = Array.length arr - 1
  and mz = Array.length arr.(0) - 1
  and my = Array.length arr.(0).(0) - 1
  and mx = Array.length arr.(0).(0).(0) - 1 in
  cartesian4 (diagonal4 [-1;0;1])
  |> filter (fun (i, j, k, l) -> (i, j, k, l) <> (0, 0, 0, 0) && 0 <= x + i && x + i <= mx && 0 <= y + j && y + j <= my && 0 <= z + k && z + k <= mz && 0 <= w + l && w + l <= mw)
  |> map (fun (i, j, k, l) -> arr.(w + l).(z + k).(y + j).(x + i))
  |> fold_left (fun active y -> match y with
                | '#' -> active + 1
                | _ -> active) 0 

let safe_get4 default w z y x arr =
  try arr.(w).(z).(y).(x) with Invalid_argument _ -> default

let update4 x y z w arr = 
  let cell = safe_get4 '.' w z y x arr in
  match cell, neighbors4 x y z w arr with
    | '.', 3 -> '#'
    | '#', 2 | '#', 3 -> '#'
    | _ -> '.'

let next_size4 arr = 
  let mw = Array.length arr + 2
  and mz = Array.length arr.(0) + 2
  and my = Array.length arr.(0).(0) + 2
  and mx = Array.length arr.(0).(0).(0) + 2 in
  let new_arr = Array.init mw (fun _ -> Array.init mz (fun _ -> (Array.make_matrix my mx '.'))) in
  Array.iteri (fun l w ->
    Array.iteri (fun k z ->
      Array.iteri (fun j y ->
        Array.iteri (fun i x -> 
          new_arr.(l).(k).(j).(i) <- safe_get4 '.' (l - 1) (k - 1) (j - 1) (i - 1) arr
        ) y) z) w) new_arr;
  new_arr

let mapi4d f arr = 
  let mw = Array.length arr
  and mz = Array.length arr.(0)
  and my = Array.length arr.(0).(0)
  and mx = Array.length arr.(0).(0).(0) in
  let new_arr = Array.init mw (fun _ -> Array.init mz (fun _ -> (Array.make_matrix my mx (f 0 0 0 0 arr)))) in
  Array.iteri (fun l w ->
    Array.iteri (fun k z ->
      Array.iteri (fun j y ->
        Array.iteri (fun i _ -> 
          new_arr.(l).(k).(j).(i) <- (f i j k l arr)
        ) y) z) w ) arr;
  new_arr

let step4 arr = 
  mapi4d update4 (next_size4 arr)

let count_active4 arr = 
  let w = Array.length arr - 1
  and z = Array.length arr.(0) - 1
  and y = Array.length arr.(0).(0) - 1
  and x = Array.length arr.(0).(0).(0) - 1 in
  let count = ref 0 in
  for l = 0 to w do
    for k = 0 to z do
      for j = 0 to y do
        for i = 0 to x do
          if arr.(l).(k).(j).(i) = '#' then count := !count + 1
  done; done; done; done; !count

let naloga2 string = 
  let current = ref (Array.make 1 (format string)) in
  for i = 1 to 6 do
    current := step4 !current
  done; !current
  |> count_active4
  |> string_of_int


let day = "17"
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
