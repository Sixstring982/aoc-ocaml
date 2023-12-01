[@@@warning "-69"]
[@@@warning "-32"]

open Import

let year = 2015
let day = 17

let parse_numbers : string -> int list =
  List.map int_of_string
  << List.filter (( < ) 0 << String.length)
  << String.split_on_char '\n'

let rec subsets : 'a list -> 'a list Seq.t = function
  | [] -> fun () -> Nil
  | [ x ] -> List.to_seq [ [ x ]; [] ]
  | x :: xs ->
      let sets_without = subsets xs in
      let sets_with = Seq.map (fun xs -> x :: xs) sets_without in
      Seq.concat (List.to_seq [ sets_with; sets_without ])

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let numbers = parse_numbers input in
    let subsets = subsets numbers in
    let matching_subsets = Seq.filter (fun s -> Int.sum s = 150) subsets in
    Ok (string_of_int @@ Seq.length matching_subsets)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let numbers = parse_numbers input in
    let subsets = subsets numbers in
    let matching_subsets = List.of_seq @@ Seq.filter (fun s -> Int.sum s = 150) subsets in
    let min_length = Int.minimum @@ List.map List.length matching_subsets in
    let min_subsets = List.filter ((=) min_length << List.length) matching_subsets in
    Ok (string_of_int @@ List.length min_subsets)
end
