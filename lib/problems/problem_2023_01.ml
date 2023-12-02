let year = 2023
let day = 1

module Char = struct
  include Char

  let zero = int_of_char '0'
  let nine = int_of_char '9'
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let numbers =
      input |> String.split_on_char '\n'
      |> List.filter (fun s -> String.length s > 0)
      |> List.map Core.String.to_list
      |> List.map
           (List.filter (fun c ->
                int_of_char c >= Char.zero && int_of_char c <= Char.nine))
      |> List.map (fun (xs : char list) ->
             match Core.List.(hd xs, last xs) with
             | None, None -> failwith "Empty list!"
             | None, Some _ -> failwith "Illegal state!"
             | Some f, None -> [ f; f ]
             | Some f, Some l -> [ f; l ])
      |> List.map Core.String.of_list
      |> List.map int_of_string
    in
    Ok (string_of_int @@ Import.Int.sum numbers)
end

module Part_2 = struct
  let rec find_first_number (input : char list) : char option =
    match input with
    | [] -> failwith "First number not found!"
    | '1' :: _ -> Some '1'
    | '2' :: _ -> Some '2'
    | '3' :: _ -> Some '3'
    | '4' :: _ -> Some '4'
    | '5' :: _ -> Some '5'
    | '6' :: _ -> Some '6'
    | '7' :: _ -> Some '7'
    | '8' :: _ -> Some '8'
    | '9' :: _ -> Some '9'
    | 'z' :: 'e' :: 'r' :: 'o' :: _ -> Some '0'
    | 'o' :: 'n' :: 'e' :: _ -> Some '1'
    | 't' :: 'w' :: 'o' :: _ -> Some '2'
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some '3'
    | 'f' :: 'o' :: 'u' :: 'r' :: _ -> Some '4'
    | 'f' :: 'i' :: 'v' :: 'e' :: _ -> Some '5'
    | 's' :: 'i' :: 'x' :: _ -> Some '6'
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some '7'
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some '8'
    | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some '9'
    | _ :: xs -> find_first_number xs

  let rec find_last_number (input : char list) : char option =
    if input = [] then failwith "Suffix not found"
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '0' ] input then
      Some '0'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '1' ] input then
      Some '1'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '2' ] input then
      Some '2'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '3' ] input then
      Some '3'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '4' ] input then
      Some '4'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '5' ] input then
      Some '5'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '6' ] input then
      Some '6'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '7' ] input then
      Some '7'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '8' ] input then
      Some '8'
    else if Core.List.is_suffix ~equal:Char.equal ~suffix:[ '9' ] input then
      Some '9'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "zero")
        input
    then Some '0'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "one")
        input
    then Some '1'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "two")
        input
    then Some '2'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "three")
        input
    then Some '3'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "four")
        input
    then Some '4'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "five")
        input
    then Some '5'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "six")
        input
    then Some '6'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "seven")
        input
    then Some '7'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "eight")
        input
    then Some '8'
    else if
      Core.List.is_suffix ~equal:Char.equal
        ~suffix:(Core.String.to_list "nine")
        input
    then Some '9'
    else find_last_number (Core.List.take input (List.length input - 1))

  let run (input : string) : (string, string) result =
    let numbers =
      input |> String.split_on_char '\n'
      |> List.filter (fun s -> String.length s > 0)
      |> List.map Core.String.to_list
      |> List.map (fun (xs : char list) ->
             match (find_first_number xs, find_last_number xs) with
             | None, None -> failwith "Empty list!"
             | None, Some _ -> failwith "Illegal state!"
             | Some f, None -> [ f; f ]
             | Some f, Some l -> [ f; l ])
      |> List.map Core.String.of_list
      |> List.map int_of_string
    in
    Ok (string_of_int @@ Import.Int.sum numbers)
end
