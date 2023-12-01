open Import

let year = 2015
let day = 14

module Reindeer = struct
  type t = { speed : int; fly_time : int; rest_time : int; points : int }

  let points (t : t) : int = t.points

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _ = String.of_chars <$> many1 ascii_letter in
    let* _ = string " can fly " in
    let* speed = unsigned_int in
    let* _ = string " km/s for " in
    let* fly_time = unsigned_int in
    let* _ = string " seconds, but then must rest for " in
    let* rest_time = unsigned_int in
    let* _ = string " seconds.\n" in
    return { speed; fly_time; rest_time; points = 0 }

  let parse_all : t list Angstrom.t = Angstrom.(many1 parse)

  let fly_for ~(seconds : int) (t : t) : int =
    let period_length = t.fly_time + t.rest_time in
    let periods = seconds / period_length in
    let mid_period =
      let leftover = Int.rem seconds period_length in
      Int.min leftover t.fly_time
    in
    t.speed * ((periods * t.fly_time) + mid_period)

  let race_step (seconds : int) (ts : t list) : t list =
    let max_distance = Int.maximum @@ List.map (fly_for ~seconds) ts in
    List.map
      (fun t ->
        let distance = fly_for ~seconds t in
        if distance = max_distance then { t with points = t.points + 1 } else t)
      ts

  let race_for ~(seconds : int) : t list -> t list =
    List.iterate_i (seconds - 1) (fun n ts -> race_step (n + 1) ts)
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ reindeer =
      Angstrom.parse_string ~consume:All Reindeer.parse_all input
    in
    Ok
      (reindeer
      |> List.map (Reindeer.fly_for ~seconds:2503)
      |> Int.maximum |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ reindeer =
      Angstrom.parse_string ~consume:All Reindeer.parse_all input
    in
    let reindeer = Reindeer.race_for ~seconds:2503 reindeer in
    Ok (reindeer |> List.map Reindeer.points |> Int.maximum |> string_of_int)
end
