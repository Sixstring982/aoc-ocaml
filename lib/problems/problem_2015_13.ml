open Import

let year = 2015
let day = 13

module Person = struct
  module Name = struct
    type t = string

    let compare : t -> t -> int = String.compare
    let equal : t -> t -> bool = String.equal

    let parse : t Angstrom.t =
      let open Angstrom in
      let* first = uppercase_ascii in
      let* rest = many1 lowercase_ascii in
      return @@ String.of_chars (first :: rest)
  end

  module Preference = struct
    type t = { name : Name.t; amount : int }

    let parse : t Angstrom.t =
      let open Angstrom in
      let* _ = string " would " in
      let* direction = string "gain" <|> string "lose" in
      let* _ = char ' ' in
      let* amount = unsigned_int in
      let* _ = string " happiness units by sitting next to " in
      let* name = Name.parse in
      let* _ = string ".\n" in
      return
      @@
      match direction with
      | "gain" -> { name; amount }
      | "lose" -> { name; amount = -amount }
      | _ -> failwith "Unreachable"
  end

  type t = { name : Name.t; preferences : Preference.t list }

  let parse_preference : (Name.t * Preference.t) Angstrom.t =
    let open Angstrom in
    let* preferer = Name.parse in
    let* preference = Preference.parse in
    return (preferer, preference)

  let parse_all_preferences : t list Angstrom.t =
    let open Angstrom in
    let module List = List.Ord(Name) in
    let module Map = List.Map in
    let* preferences = many1 parse_preference in
    let preferences : Preference.t list Map.t = 
      List.group_by fst preferences in
    return
    @@ List.map (fun (name, preferences) -> { name; preferences }) preferences
end

module Table = struct
  type t = Person.t array
end

module Part_1 = struct
  let run (input : string) : (string, string) result = Ok input
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
