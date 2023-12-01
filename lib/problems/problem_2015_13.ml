open Import

let year = 2015
let day = 13

module Person = struct
  module Name : sig
    type t

    val compare : t -> t -> int
    val parse : t Angstrom.t
  end = struct
    type t = string

    let compare : t -> t -> int = String.compare

    let parse : t Angstrom.t =
      let open Angstrom in
      let* first = uppercase_ascii in
      let* rest = many1 lowercase_ascii in
      return @@ String.of_chars (first :: rest)
  end

  module Preference = struct
    type t = { name : Name.t; amount : int }

    let name (t : t) : Name.t = t.name
    let amount (t : t) : int = t.amount

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

  let parse_preference : (Name.t * Preference.t) Angstrom.t =
    let open Angstrom in
    let* preferer = Name.parse in
    let* preference = Preference.parse in
    return (preferer, preference)
end

module Party = struct
  module Table = Table.Make (Person.Name) (Person.Name)

  type t = int Table.t

  let parse : t Angstrom.t =
    let open Angstrom in
    let* preferences = many1 Person.parse_preference in
    return
    @@ Table.map (Person.Preference.amount << snd)
    @@ Table.group_by fst (Person.Preference.name << snd) preferences

  let preference (t : t) (of' : Person.Name.t) ~(for' : Person.Name.t) :
      (int, string) result =
    match Table.find_opt of' for' t with
    | None -> Error "Preference not found!"
    | Some x -> Ok x

  let all_arangements : t -> Person.Name.t list list =
    List.permutations << Table.keys

  let evaluate_pair (t : t) (left : Person.Name.t) (right : Person.Name.t) :
      (int, string) result =
    let@ p1 = preference t left ~for':right in
    let@ p2 = preference t right ~for':left in
    Ok (p1 + p2)

  let evaluate_arrangement (t : t) (arrangement : Person.Name.t list) :
      (int list, string) result =
    match arrangement with
    | [] -> Error "Party too small"
    | [ _ ] -> Error "Party too small"
    | first :: _ ->
        let rec go (acc : int list) = function
          | [] -> Error "Party too small!"
          | [ x ] ->
              let@ pair = evaluate_pair t x first in
              Ok (pair :: acc)
          | x :: y :: xs ->
              let@ seat = evaluate_pair t x y in
              (go [@tailcall]) (seat :: acc) (y :: xs)
        in
        go [] arrangement
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ party = Angstrom.parse_string ~consume:All Party.parse input in
    let arrangements = Party.all_arangements party in
    let@ arrangement_scores =
      arrangements
      |> List.map (Party.evaluate_arrangement party)
      |> Result.Monad2.sequence
    in
    let max_score = arrangement_scores |> List.map Int.sum |> Int.maximum in
    Ok (string_of_int max_score)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ party = Angstrom.parse_string ~consume:All Party.parse input in
    let arrangements = Party.all_arangements party in
    let@ arrangement_scores =
      arrangements
      |> List.map (Party.evaluate_arrangement party)
      |> Result.Monad2.sequence
    in
    let max_score = 
      arrangement_scores 
      |> List.map Int.(sum << remove_first_by minimum)
      |> Int.maximum in
    Ok (string_of_int max_score)
end
