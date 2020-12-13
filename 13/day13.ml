open List
#use "topfind"
#require "core"

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
  let lst = string_to_list string in
  let n = int_of_string (hd lst)
  and buses = hd (tl lst) 
  |> String.split_on_char ','
  |> filter (fun x -> x <> "x")
  |> map int_of_string
  in
  (n, buses)

let format' string = 
  let lst = string_to_list string in 
  hd (tl lst)
  |> String.split_on_char ','
  |> map (fun x -> if x = "x" then 0 else int_of_string x)

let naloga1 string = ""

let small = "939\n7,13,x,x,59,x,31,19"

let naloga1 string = 
  let n, buses = format string in
  let rec loop i =
    match map2 (fun x y -> (i mod x, y)) buses buses
    |> filter (fun (x, y) -> x = 0)
    with 
    | [] -> loop (i + 1)
    | (x, y)::xs -> (i - n) * y
  in loop n |> string_of_int

(*https://rosettacode.org/wiki/Chinese_remainder_theorem#OCaml*)
open Core
open Option.Monad_infix

let rec egcd a b =
   if b = 0 then (1, 0)
   else
      let q = a/b and r = a mod b in
      let (s, t) = egcd b r in
         (t, s - q*t)
 
let mod_inv a b =
   let (x, y) = egcd a b in
      if a*x + b*y = 1 then Some x else None
 
let calc_inverses ns ms =
   let rec list_inverses ns ms l =
      match (ns, ms) with
         | ([], []) -> Some l
         | ([], _)
         | (_, []) -> assert false
         | (n::ns, m::ms) ->
            let inv = mod_inv n m in
               match inv with
                  | None -> None
                  | Some v -> list_inverses ns ms (v::l)
   in
      list_inverses ns ms [] >>= fun l -> Some (List.rev l)

let chinese_remainder congruences =
   let (residues, modulii) = List.unzip congruences in
   let mod_pi = List.reduce_exn modulii ~f:( * ) in
   let crt_modulii = List.map modulii ~f:(fun m -> mod_pi / m) in
   calc_inverses crt_modulii modulii >>=
      fun inverses ->
         Some (List.map3_exn residues inverses crt_modulii ~f:(fun a b c -> a*b*c)
               |> List.reduce_exn ~f:(+)
               |> fun n -> let n' = n mod mod_pi in if n' < 0 then n' + mod_pi else n')

open Stdlib

let tiny = "0\n17,x,13,19"

let naloga2 string = 
  let buses = format' string in
  match mapi (fun i x -> if x = 0 then (0, 0) else (-i, x)) buses
  |> filter (fun x -> x <> (0, 0))
  |> chinese_remainder
  with
  | None -> failwith "yikerino"
  | Some n -> string_of_int n

let day = "13"
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
