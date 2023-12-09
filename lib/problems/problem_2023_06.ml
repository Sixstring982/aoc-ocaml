[@@@warning "-69"]

open Import

let year = 2023
let day = 6

module Race = (* {{{ *) struct
  type t = { time : int; distance : int }

  let wins (t : t) (held_time : int) : bool =
    let go_time = t.time - held_time in
    go_time * held_time > t.distance

  let rec bisect ~(low : int) ~(high : int)
      ~(f : int -> [ `Too_high | `Too_low | `Just_right ]) : int =
    let mid = (low + high) / 2 in
    match f mid with
    | `Just_right -> mid
    | `Too_low -> (bisect [@tailcall]) ~low:mid ~high ~f
    | `Too_high -> (bisect [@tailcall]) ~low ~high:mid ~f

  let find_low_win (t : t) : int =
    let wins = wins t in
    bisect ~low:1 ~high:(t.time / 2) ~f:(fun x ->
        match (wins x, wins (x + 1)) with
        | false, false -> `Too_low
        | false, true -> `Just_right
        | true, true -> `Too_high
        | true, false -> failwith "Unexpected: find_low_min")

  let find_high_max (t : t) : int =
    let wins = wins t in
    bisect ~low:(t.time / 2) ~high:t.time ~f:(fun x ->
        match (wins x, wins (x + 1)) with
        | true, true -> `Too_low
        | true, false -> `Just_right
        | false, false -> `Too_high
        | false, true -> failwith "Unexpected: find_low_min")

  let count_ways_to_win_fast (t : t) : int =
    let low_min = find_low_win t in
    let high_max = find_high_max t in
    high_max - low_min

  let count_ways_to_win (t : t) : int =
    let rec go (acc : int) (next : int) : int =
      match next with
      | n when n >= t.distance -> acc
      | n -> go (acc + if wins t n then 1 else 0) (next + 1)
    in
    go 0 1
end
(* }}} *)

module Angstrom = (* {{{ *) struct
  include Angstrom

  let prefixed_unsigned_int : int t =
    let* _ = many1 (char ' ') in
    unsigned_int

  let times : int list t =
    let* _ = string "Time:" in
    let* ns = many1 prefixed_unsigned_int in
    let* _ = char '\n' in
    return ns

  let distances : int list t =
    let* _ = string "Distance:" in
    let* ns = many1 prefixed_unsigned_int in
    let* _ = char '\n' in
    return ns

  let races : Race.t list t =
    let* times = times in
    let* distances = distances in
    match List.zip times distances with
    | Unequal_lengths -> fail "Times and distances are of unequal lengths"
    | Ok xs ->
        return @@ List.map ~f:(fun (a, b) -> Race.{ time = a; distance = b }) xs
end
(* }}} *)

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ races = Angstrom.(parse_string ~consume:All races input) in
    let ways_to_win = List.map ~f:Race.count_ways_to_win races in
    Ok (string_of_int @@ Int.product ways_to_win)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ races = Angstrom.(parse_string ~consume:All races input) in
    let ways_to_win = List.map ~f:Race.count_ways_to_win_fast races in
    Ok (string_of_int @@ Int.product ways_to_win)
end
