open Import

let year = 2015
let day = 10

module Look_and_say : sig
  val apply : char list -> char list
end = struct
  module Acc = struct
    type t =
      | Empty
      | Counting of { char : char; count : int; chars : Buffer.t }
  end

  let apply (input : char list) : char list =
    let rec go (acc : Acc.t) : char list -> Acc.t = function
      | [] -> (
          match acc with
          | Empty -> acc
          | Counting { char; count; chars } ->
              Buffer.add_string chars @@ string_of_int count;
              Buffer.add_char chars char;
              Counting { char; count; chars })
      | x :: xs ->
          let acc =
            match acc with
            | Empty ->
                Acc.Counting { char = x; count = 1; chars = Buffer.create 16 }
            | Counting { char; count; chars } ->
                if x = char then Counting { char; count = count + 1; chars }
                else (
                  Buffer.add_string chars @@ string_of_int count;
                  Buffer.add_char chars char;
                  Counting { char = x; count = 1; chars })
          in
          go acc xs
    in
    let result = go Empty input in
    match result with
    | Empty -> []
    | Counting { chars; _ } -> String.to_list @@ Buffer.contents chars
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    input |> String.trim |> String.to_list
    |> List.iterate 40 Look_and_say.apply
    |> List.length |> string_of_int |> Result.ok
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    input |> String.trim |> String.to_list
    |> List.iterate 50 Look_and_say.apply
    |> List.length |> string_of_int |> Result.ok
end
