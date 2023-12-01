open Import

let year = 2015
let day = 19

module Rule = struct
  type t = { _replace : char list; _with' : char list }

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _replace = many1 ascii_letter in
    let* _ = string " => " in
    let* _with' = many1 ascii_letter in
    let* _ = char '\n' in
    return { _replace; _with' }
end

let parse_input : (Rule.t list * char list) Angstrom.t =
  let open Angstrom in
  let* rules = many1 Rule.parse in
  let* _ = char '\n' in
  let* molecule = many1 ascii_letter in
  let* _ = char '\n' in
  return (rules, molecule)

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ _rules, molecule =
      Angstrom.parse_string ~consume:All parse_input input
    in
    Ok (String.of_chars molecule)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
