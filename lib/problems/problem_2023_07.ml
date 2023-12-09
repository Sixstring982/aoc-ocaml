[@@@warning "-69"]

open Import

let year = 2023
let day = 7

module Card = struct
  type t = char

  let rank : t -> int = function
    | '2' -> 0
    | '3' -> 1
    | '4' -> 2
    | '5' -> 3
    | '6' -> 4
    | '7' -> 5
    | '8' -> 6
    | '9' -> 7
    | 'T' -> 8
    | 'J' -> 9
    | 'Q' -> 10
    | 'K' -> 11
    | 'A' -> 12
    | c -> failwith @@ Fmt.str "Invalid card char: %c" c

  let rank_with_jokers : t -> int = function
    | 'J' -> 0
    | '2' -> 1
    | '3' -> 2
    | '4' -> 3
    | '5' -> 4
    | '6' -> 5
    | '7' -> 6
    | '8' -> 7
    | '9' -> 8
    | 'T' -> 9
    | 'Q' -> 10
    | 'K' -> 11
    | 'A' -> 12
    | c -> failwith @@ Fmt.str "Invalid card char: %c" c
end

module Hand = struct
  module Type = struct
    type t =
      | Five_of_a_kind
      | Four_of_a_kind
      | Full_house
      | Three_of_a_kind
      | Two_pair
      | One_pair
      | High_card

    let strength : t -> int = function
      | Five_of_a_kind -> 6
      | Four_of_a_kind -> 5
      | Full_house -> 4
      | Three_of_a_kind -> 3
      | Two_pair -> 2
      | One_pair -> 1
      | High_card -> 0

    module Multimap = Multimap.Make (Char)

    let of_cards (cards : Card.t list) : t =
      let mem = List.mem ~equal:Int.equal in
      let counts =
        cards |> Multimap.group_by id |> Multimap.entries |> List.map ~f:snd
        |> List.map ~f:List.length
      in
      if mem counts 5 then Five_of_a_kind
      else if mem counts 4 then Four_of_a_kind
      else if mem counts 3 then
        if mem counts 2 then Full_house else Three_of_a_kind
      else if List.length (List.filter ~f:(Int.equal 2) counts) = 2 then
        Two_pair
      else if mem counts 2 then One_pair
      else High_card

    let of_cards_with_jokers (cards : Card.t list) : t =
      let mem = List.mem ~equal:Int.equal in
      let joker_count = List.length @@ List.filter ~f:(Char.equal 'J') cards in
      let counts =
        cards
        |> List.filter ~f:(not << Char.equal 'J')
        |> Multimap.group_by id |> Multimap.entries
        |> List.map ~f:(fun (k, v) -> (k, List.length v))
      in
      let max_without_jokers =
        counts
        |> List.filter ~f:(fun ((k, _) : char * int) -> not (Char.equal k 'J'))
        |> List.max_by (fun (c1, v1) (c2, v2) ->
               let c = Int.compare v1 v2 in
               if c <> 0 then c
               else
                 Int.compare (Card.rank_with_jokers c1)
                   (Card.rank_with_jokers c2))
        |> Option.value ~default:('J', 0)
      in
      let counts =
        List.Assoc.add counts ~equal:Char.equal (fst max_without_jokers)
          (snd max_without_jokers + joker_count)
      in
      let counts = List.map ~f:snd counts in
      if mem counts 5 then Five_of_a_kind
      else if mem counts 4 then Four_of_a_kind
      else if mem counts 3 then
        if mem counts 2 then Full_house else Three_of_a_kind
      else if List.length (List.filter ~f:(Int.equal 2) counts) = 2 then
        Two_pair
      else if mem counts 2 then One_pair
      else High_card

  end

  type t = { cards : Card.t list; type' : Type.t; second_ordering_rank : int }

  let compare (a : t) (b : t) : int =
    let type_comparison =
      Int.compare (Type.strength a.type') (Type.strength b.type')
    in
    if type_comparison <> 0 then type_comparison
    else Int.compare a.second_ordering_rank b.second_ordering_rank

  let second_ordering_rank_of_cards (cards : Card.t list) : int =
    let rec go (acc : int) = function
      | [] -> acc
      | x :: xs -> go ((acc * 13) + Card.rank x) xs
    in
    go 0 cards

  let second_ordering_rank_of_cards_with_jokers (cards : Card.t list) : int =
    let rec go (acc : int) = function
      | [] -> acc
      | x :: xs -> go ((acc * 13) + Card.rank_with_jokers x) xs
    in
    go 0 cards
end

module Round = struct
  type t = { hand : Hand.t; bid : int }

  let compare (a : t) (b : t) : int = Hand.compare a.hand b.hand
end

module Angstrom = (* {{{ *) struct
  include Angstrom

  let card : Card.t t =
    choice
    @@ List.map ~f:char
         [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'K'; 'Q'; 'A' ]

  let hand : Hand.t t =
    let* cards = count 5 card in
    let type' = Hand.Type.of_cards cards in
    let second_ordering_rank = Hand.second_ordering_rank_of_cards cards in
    return Hand.{ cards; type'; second_ordering_rank }

  let hand_with_jokers : Hand.t t =
    let* cards = count 5 card in
    let type' = Hand.Type.of_cards_with_jokers cards in
    let second_ordering_rank =
      Hand.second_ordering_rank_of_cards_with_jokers cards
    in
    return Hand.{ cards; type'; second_ordering_rank }

  let round : Round.t t =
    let* hand = hand in
    let* _ = char ' ' in
    let* bid = unsigned_int in
    let* _ = char '\n' in
    return Round.{ hand; bid }

  let round_with_jokers : Round.t t =
    let* hand = hand_with_jokers in
    let* _ = char ' ' in
    let* bid = unsigned_int in
    let* _ = char '\n' in
    return Round.{ hand; bid }
end
(* }}} *)

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ rounds = Angstrom.(parse_string ~consume:All (many1 round) input) in
    let ranked_rounds =
      match
        List.zip
          (List.range 1 (List.length rounds + 1))
          (List.sort ~compare:Round.compare rounds)
      with
      | Unequal_lengths -> failwith "Unequal lengths"
      | Ok xs -> xs
    in
    let answer =
      Int.sum
      @@ List.map ~f:(fun (rank, round) -> rank * round.bid) ranked_rounds
    in
    Ok (string_of_int answer)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ rounds =
      Angstrom.(parse_string ~consume:All (many1 round_with_jokers) input)
    in
    let ranked_rounds =
      match
        List.zip
          (List.range 1 (List.length rounds + 1))
          (List.sort ~compare:Round.compare rounds)
      with
      | Unequal_lengths -> failwith "Unequal lengths"
      | Ok xs -> xs
    in
    let answer =
      Int.sum
      @@ List.map ~f:(fun (rank, round) -> rank * round.bid) ranked_rounds
    in
    Ok (string_of_int answer)
end
