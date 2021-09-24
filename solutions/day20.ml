#use "topfind"
#require "str"
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
  String.split_on_char '\n' string
  |> List.filter (fun s -> s <> "")

let explode input = input |> String.to_seq |> List.of_seq
let small = "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."

let get_id lst =
  (String.sub (hd lst) 0 (String.length (hd lst) - 1), tl lst)

let list_of_lines_to_array lst = 
  let arr = Array.make_matrix (length lst) (length lst) '.' in
  List.iteri (fun i x -> List.iteri (fun j y -> arr.(i).(j) <- y) x) (map explode lst);
  arr

let format string =
  Str.split (Str.regexp "\n\n") string
  |> map (String.split_on_char '\n')
  |> map get_id
  |> map (fun (x, y)-> (int_of_string @@ hd @@ tl @@ String.split_on_char ' ' (String.sub x 0 (String.length x)), list_of_lines_to_array y))

let vert_mirror matrix = 
  let l = Array.length matrix in
  let retmatrix = Array.make_matrix l l matrix.(0).(0) in
  Array.iteri (fun i x -> retmatrix.(l - i - 1) <- x) matrix;
  retmatrix

let rotate_right_once matrix = 
  let l = Array.length matrix in
  let retmatrix = Array.make_matrix l l matrix.(0).(0) in
  Array.iteri (fun i x -> Array.iteri (fun j y -> retmatrix.(j).(l - i - 1) <- y) x) matrix;
  retmatrix

let rec app_n n f x = match n with
  | 0 -> x
  | 1 -> f x
  | n -> f (app_n (pred n) f x)

let all_rotations matrix =
  let rec rotator aux rot mir = match rot, mir with
    | 4, _ -> aux
    | x, 0 -> rotator ((vert_mirror (app_n x rotate_right_once matrix))::aux) rot 1
    | x, 1 -> rotator ((app_n x rotate_right_once matrix)::aux) (succ rot) 0
    | _ -> failwith "da ocaml neha težit"
  in rotator [] 0 0

let arr_rev x = Array.of_list @@ rev @@ Array.to_list x

let get_edges matrix = 
  let l = Array.length matrix in
  let sides = Array.make_matrix 8 l matrix.(0).(0) in
  sides.(0) <- matrix.(0);
  sides.(2) <- matrix.(l - 1);
  for i = 0 to l - 1 do sides.(1).(i) <- matrix.(i).(0); done;
  for i = 0 to l - 1 do sides.(3).(i) <- matrix.(i).(l - 1); done;
  for i = 4 to 7 do
    sides.(i) <- arr_rev sides.(i - 4);
  done;
  Array.to_list sides

let find_matching (id, matrix) id_matrices = 
  let edges = get_edges matrix in
  id_matrices
  |> filter (fun (i, m) -> exists (fun x -> mem x edges) (get_edges m))
  |> map fst
  |> filter (fun x -> x <> id)

let naloga1 string =
  let id_matrices = format string in
  map (fun x -> fst x, find_matching x id_matrices) id_matrices
  |> filter (fun (x, y) -> length y = 2)
  |> map fst
  |> fold_left ( * ) 1
  |> string_of_int

let get_matrix_right (id, matrix) matrices =
  let right_edge = nth (get_edges matrix) 3 in 
  let id_next = filter (fun (i, x) -> i <> id && mem right_edge (get_edges x)) matrices
    |> hd in
  let final = all_rotations (snd id_next)
    |> filter (fun x -> right_edge = nth (get_edges x) 1)
    |> hd in
  (fst id_next, final)

let get_matrix_down (id, matrix) matrices =
  let down_edge = nth (get_edges matrix) 2 in 
  let id_next = filter (fun (i, x) -> i <> id && mem down_edge (get_edges x)) matrices
    |> hd in
  let final = all_rotations (snd id_next)
    |> filter (fun x -> down_edge = x.(0))
    |> hd in
  (fst id_next, final)

let trim_matrix matrix = 
  let l = Array.length matrix in
  let new_matrix = Array.make_matrix (l - 2) (l - 2) matrix.(0).(0) in
  Array.iteri (fun i x ->
    Array.iteri (fun j _ -> new_matrix.(i).(j) <- matrix.(i + 1).(j + 1)) x) new_matrix;
  new_matrix

let concat_matrix_matrix matrix_matrix = 
  let l = Array.length matrix_matrix in
  let l' = Array.length matrix_matrix.(0).(0) in
  let ret_matrix = Array.make_matrix (l * l') (l * l') matrix_matrix.(0).(0).(0).(0) in
  Array.iteri (fun i x ->
    Array.iteri (fun j y ->
      Array.iteri (fun k z ->
        Array.iteri (fun f w -> 
          ret_matrix.(i * l' + k).(j * l' + f) <- w;
        ) z) y) x) matrix_matrix;
  ret_matrix

let monster_no chart =
  let monster = Array.of_list (map Array.of_list (map explode ["                  # "; "#    ##    ##    ###"; " #  #  #  #  #  #   "])) in
  let monster_x, monster_y = Array.length monster, Array.length monster.(0) in
  let hit_squares = Array.make_matrix (Array.length chart) (Array.length chart.(0)) false in

  let count_subsection subarray startx starty =
    let succeeded = ref true in
    let hit = ref [] in
    Array.iteri (fun i x ->
      Array.iteri (fun j y ->
        if y = '#' then
          (if subarray.(i).(j) = '#' then hit := !hit @ [(i, j)] else succeeded := false)
      ) x) monster;
    if !succeeded then 
      List.iter (fun (x, y) -> hit_squares.(startx + x).(starty + y) <- true) !hit; 
    in
  for i = 0 to Array.length chart - monster_x do
    for j = 0 to Array.length chart.(0) - monster_y do
      count_subsection (Array.map (fun z -> Array.sub z j monster_y) (Array.sub chart i monster_x)) i j;
    done;
  done;
  let final_count = ref 0 in
  Array.iter (fun x ->
    Array.iter (fun y ->
      if y then incr final_count )x ) hit_squares;
  !final_count

let naloga2 string =
  let matrices = format string in
  let mapsize = int_of_float ((float_of_int @@ length matrices) ** (0.5)) in
  let first = hd @@ filter (fun (x, y) -> length (find_matching (x, y) matrices) = 2) matrices |> (fun (x, y) -> (x, rotate_right_once y)) in
  let big_matrix = Array.make_matrix mapsize mapsize first in
  for i = 1 to mapsize - 1 do
    big_matrix.(0).(i) <- get_matrix_right big_matrix.(0).(i - 1) matrices;
  done;
  for j = 0 to mapsize - 1 do
    for i = 1 to mapsize - 1 do
      big_matrix.(i).(j) <- get_matrix_down big_matrix.(i - 1).(j) matrices;
    done;
  done;
  let matrix_matrix = Array.map (Array.map snd) big_matrix in
  let chart = concat_matrix_matrix @@ Array.map (Array.map trim_matrix) matrix_matrix in
  let charts = all_rotations chart in
  let monster_counts = map monster_no charts in
  let all_count = Array.fold_left (+) 0 (Array.map (Array.fold_left (fun x y -> if y = '#' then succ x else x) 0) chart) in
  fold_left (fun x y -> if y = 0 then x else all_count - y) 0 monster_counts
  |> string_of_int

let day = "20"
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
