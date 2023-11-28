open! Import

let year = 2015
let day = 12

module Part_1 = struct
  let rec find_number_sum : Yojson.Basic.t -> float = function
    | `Null -> 0.
    | `Bool _ -> 0.
    | `String _ -> 0.
    | `Float f -> f
    | `Int i -> float_of_int i
    | `Assoc o -> find_number_sum (`List (List.map snd o))
    | `List js -> Float.sum @@ List.map find_number_sum js

  let run (input : string) : (string, string) result =
    let json = Yojson.Basic.from_string (String.trim input) in
    let sum = find_number_sum json in
    Ok (string_of_int @@ int_of_float sum)
end

module Part_2 = struct
  let rec find_number_sum : Yojson.Basic.t -> float = function
    | `Null -> 0.
    | `Bool _ -> 0.
    | `String _ -> 0.
    | `Float f -> f
    | `Int i -> float_of_int i
    | `Assoc o -> (
        match List.find_opt (fun (_, v) -> v = `String "red") o with
        | None -> find_number_sum (`List (List.map snd o))
        | Some _ -> 0.)
    | `List js -> Float.sum @@ List.map find_number_sum js

  let run (input : string) : (string, string) result =
    let json = Yojson.Basic.from_string (String.trim input) in
    let sum = find_number_sum json in
    Ok (string_of_int @@ int_of_float sum)
end
