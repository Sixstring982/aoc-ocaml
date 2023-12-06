[@@@warning "-69"]

open Import

let year = 2023
let day = 5

module Almanac = struct
  type t = { seeds : int list; maps : (int -> int) list }

  let eval_map_row : int * int * int -> int -> int option =
   fun (dst_start, src_start, length) n ->
    if n >= src_start && n < src_start + length then
      Some (dst_start + (n - src_start))
    else None

  let rec map_of_rows : (int * int * int) list -> int -> int =
   fun maps src ->
    match maps with
    | [] -> src
    | x :: xs -> (
        match eval_map_row x src with
        | None -> (map_of_rows [@tailcall]) xs src
        | Some dst -> dst)

  let eval_seed (maps : (int -> int) list) (seed : int) : int =
    List.fold maps ~init:seed ~f:(fun seed next ->
        let result = next seed in
        result)

  let eval_seeds (maps : (int -> int) list) (seeds : int list) : int list =
    let ( let* ) x f = List.concat_map ~f x in
    let* seed = seeds in
    [ eval_seed maps seed ]

  let min_in_range (start: int) (end_exclusive: int) ~(f: int -> int): int =
    let rec go (acc: int) (next: int): int =
      if next >= end_exclusive then acc
      else 
        let n = f next in
        (go [@tailcall]) (min acc n) (next + 1)
    in go Int.max_value start

  let eval_seed_ranges (t : t) : int =
    let rec go (min_so_far : int) = function
      | [] -> min_so_far
      | [ _ ] -> failwith "Odd number of seeds"
      | s1 :: s2 :: ss ->
          Format.printf "Computing range %d, %d\n%!" s1 s2;
          let min_of_range = min_in_range s1 (s1 + s2) ~f:(eval_seed t.maps) in
          (go [@tailcall]) (Int.min min_so_far min_of_range) ss
    in
    go Int.max_value t.seeds

  let eval_seeds (t : t) : int list = eval_seeds t.maps t.seeds
end

module Angstrom = struct (* {{{ *)
  include Angstrom

  let seeds : int list t =
    let* _ = string "seeds: " in
    let* seeds = sep_by1 (char ' ') unsigned_int in
    let* _ = string "\n\n" in
    return seeds

  let map_row : (int * int * int) t =
    let* dest_start = unsigned_int in
    let* _ = char ' ' in
    let* src_start = unsigned_int in
    let* _ = char ' ' in
    let* length = unsigned_int in
    let* _ = char '\n' in
    return (dest_start, src_start, length)

  let almanac_map : (int -> int) t =
    let* _name = many1 (ascii_letter <|> char '-') in
    let* _ = string " map:\n" in
    let* rows = many1 map_row in
    return @@ Almanac.map_of_rows rows

  let almanac : Almanac.t t =
    let* seeds = seeds in
    let* maps = sep_by1 (char '\n') almanac_map in
    return Almanac.{ seeds; maps }
end (* }}} *)

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ almanac = Angstrom.(parse_string ~consume:All almanac input) in
    let results = Almanac.eval_seeds almanac in
    Ok (string_of_int @@ Int.minimum results)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ almanac = Angstrom.(parse_string ~consume:All almanac input) in
    let result = Almanac.eval_seed_ranges almanac in
    Ok (string_of_int result)
end
