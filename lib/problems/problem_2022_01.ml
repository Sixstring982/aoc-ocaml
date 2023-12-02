open Import

let year = 2022
let day = 1

module Elf = struct
  type t = { total_calories : int }

  let total_calories (t : t) : int = t.total_calories

  let parse : t Angstrom.t =
    let open Angstrom in
    let* calories = sep_by1 (char '\n') unsigned_int in
    let total_calories = Import.Int.sum calories in
    return { total_calories }

  let parse_all : t list Angstrom.t =
    let open Angstrom in
    let* elves = sep_by1 (string "\n\n") parse in
    let* _ = char '\n' in
    return elves
end

let parse_input (input : string) : (Elf.t list, string) result =
  Angstrom.parse_string ~consume:All Elf.parse_all input

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ elves = parse_input input in
    let max_calories =
      List.(rev @@ sort ~compare:Int.compare @@ map ~f:Elf.total_calories elves)
    in
    match max_calories with
    | [] -> Error "Not enough elves."
    | x :: _ -> Ok (string_of_int x)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ elves = parse_input input in
    let sorted_calories =
      elves
      |> List.map ~f:Elf.total_calories
      |> List.sort ~compare:Int.compare
      |> List.rev
    in
    match sorted_calories with
    | a :: b :: c :: _ -> Ok (string_of_int (a + b + c))
    | _ -> Error "Not enough elves."
end
