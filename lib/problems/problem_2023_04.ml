open Import

let year = 2023
let day = 4

module Card = struct
  type t = { numbers : int list; guesses : int list; matching_numbers : int }

  let matching_numbers ~(numbers : int list) ~(guesses : int list) : int =
    List.length
    @@ List.filter ~f:(fun x -> List.mem numbers x ~equal:Int.equal) guesses

  let points ~(numbers : int list) ~(guesses : int list) : int =
    let right_guesses = matching_numbers ~numbers ~guesses in
    if right_guesses = 0 then 0 else Int.pow 2 (max 0 right_guesses - 1)

  let points_c (card : t) : int =
    points ~numbers:card.numbers ~guesses:card.guesses

  let copies_won (xs : t list) : int =
    let module Map = Map.Make_tree (Int) in
    let module Acc = struct
      type t = { acc : int; index : int; muls : int Map.t }

      let empty : t = { acc = 0; index = 1; muls = Map.empty }
    end in
    let rec go (acc : Acc.t) = function
      | [] -> acc
      | x :: xs ->
          let multiplier =
            match Map.find acc.muls acc.index with None -> 1 | Some x -> x
          in
          let muls =
            List.fold
              (List.range (acc.index + 1) (acc.index + 1 + x.matching_numbers))
              ~init:acc.muls
              ~f:
                (Map.update ~f:(function
                  | None -> 1 + multiplier
                  | Some x -> x + multiplier))
          in
          (go [@tailcall])
            { acc = acc.acc + multiplier; index = acc.index + 1; muls }
            xs
    in
    (go Acc.empty xs).acc

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _ = string "Card" in
    let* _ = many1 (char ' ') in
    let* _id = unsigned_int in
    let* _ = string ":" in
    let* _ = many1 (char ' ') in
    let* numbers = sep_by1 (many1 (char ' ')) unsigned_int in
    let* _ = many1 (char ' ') in
    let* _ = string "|" in
    let* _ = many1 (char ' ') in
    let* guesses = sep_by1 (many1 (char ' ')) unsigned_int in
    let* _ = char '\n' in
    let matching_numbers = matching_numbers ~numbers ~guesses in
    return { numbers; guesses; matching_numbers }

  let parse_all : t list Angstrom.t =
    let open Angstrom in
    many1 parse
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ cards = Angstrom.parse_string ~consume:All Card.parse_all input in
    let total_card_points = Int.sum @@ List.map ~f:Card.points_c cards in
    Ok (string_of_int total_card_points)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ cards = Angstrom.parse_string ~consume:All Card.parse_all input in
    let total_card_points = Card.copies_won cards in
    Ok (string_of_int total_card_points)
end
