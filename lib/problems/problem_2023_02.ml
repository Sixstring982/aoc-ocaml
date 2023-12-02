open Import

let year = 2023
let day = 2

module Subset = struct
  type t = { red : int; green : int; blue : int }

  let red (t : t) : int = t.red
  let green (t : t) : int = t.green
  let blue (t : t) : int = t.blue
  let empty : t = { red = 0; green = 0; blue = 0 }

  let merge (a : t) (b : t) : t =
    { red = a.red + b.red; green = a.green + b.green; blue = a.blue + b.blue }

  let power (t : t) : int = t.red * t.green * t.blue

  let parse_pair : t Angstrom.t =
    let open Angstrom in
    let* n = unsigned_int in
    let* _ = char ' ' in
    let* color = string "red" <|> string "green" <|> string "blue" in
    match color with
    | "red" -> return { red = n; green = 0; blue = 0 }
    | "green" -> return { red = 0; green = n; blue = 0 }
    | "blue" -> return { red = 0; green = 0; blue = n }
    | s -> failwith @@ "Unreachable (invalid subset pair: " ^ s ^ ")"

  let parse : t Angstrom.t =
    let open Angstrom in
    let* pairs = sep_by1 (string ", ") parse_pair in
    return @@ List.fold pairs ~f:merge ~init:empty
end

module Game = struct
  type t = { id : int; subsets : Subset.t list }

  let id (t : t) : int = t.id

  let min_set (t : t) : Subset.t =
    let red = Int.maximum @@ List.map t.subsets ~f:Subset.red in
    let green = Int.maximum @@ List.map t.subsets ~f:Subset.green in
    let blue = Int.maximum @@ List.map t.subsets ~f:Subset.blue in
    { red; green; blue }

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _ = string "Game " in
    let* id = unsigned_int in
    let* _ = string ": " in
    let* subsets = sep_by1 (string "; ") Subset.parse in
    let* _ = char '\n' in
    return { id; subsets }

  let parse_all : t list Angstrom.t =
    let open Angstrom in
    many1 parse
end

module Part_1 = struct
  let bag_contents : Subset.t = { red = 12; green = 13; blue = 14 }

  let is_possible (t : Subset.t) : bool =
    t.red <= bag_contents.red
    && t.green <= bag_contents.green
    && t.blue <= bag_contents.blue

  let run (input : string) : (string, string) result =
    let@ games = Angstrom.parse_string ~consume:All Game.parse_all input in
    Ok
      (games
      |> List.filter ~f:(fun (g : Game.t) ->
             List.for_all ~f:is_possible g.subsets)
      |> List.map ~f:Game.id |> Int.sum |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ games = Angstrom.parse_string ~consume:All Game.parse_all input in
    Ok
      (games
      |> List.map ~f:Game.min_set
      |> List.map ~f:Subset.power
      |> Int.sum
      |> string_of_int)
end
