open Import

let year = 2015
let day = 8

module X_str = struct
  type t = { code_size : int; memory_size : int; encoded_size : int }

  let code_size (t : t) : int = t.code_size
  let memory_size (t : t) : int = t.memory_size
  let encoded_size (t : t) : int = t.encoded_size
  let empty : t = { code_size = 0; memory_size = 0; encoded_size = 0 }

  let merge (a : t) (b : t) : t =
    {
      code_size = a.code_size + b.code_size;
      memory_size = a.memory_size + b.memory_size;
      encoded_size = a.encoded_size + b.encoded_size;
    }

  let merge_all = List.fold_left merge empty

  let parse_escaped_quote : t Angstrom.t =
    let open Angstrom in
    let* _ = string {|\"|} in
    return { code_size = 2; memory_size = 1; encoded_size = 4 }

  let parse_escaped_backslash : t Angstrom.t =
    let open Angstrom in
    let* _ = string {|\\|} in
    return { code_size = 2; memory_size = 1; encoded_size = 4 }

  let parse_escaped_ascii_literal : t Angstrom.t =
    let open Angstrom in
    let* _ = string "\\x" in
    let* _ = char_of_int <$> hex_byte in
    return { code_size = 4; memory_size = 1; encoded_size = 5 }

  let lowercase_ascii_letter : t Angstrom.t =
    let open Angstrom in
    let* _ = lowercase_ascii in
    return { code_size = 1; memory_size = 1; encoded_size = 1 }

  let parse_contents : t Angstrom.t =
    let open Angstrom in
    let* content =
      many
      @@ choice
           [
             parse_escaped_quote;
             parse_escaped_backslash;
             parse_escaped_ascii_literal;
             lowercase_ascii_letter;
           ]
    in
    return @@ merge_all content

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _ = char '"' in
    let* contents = parse_contents in
    let* _ = char '"' in
    return
      {
        contents with
        code_size = contents.code_size + 2;
        encoded_size = contents.encoded_size + 6;
      }

  let parse_all : t list Angstrom.t =
    let open Angstrom in
    let* x_strs = sep_by1 (char '\n') parse in
    let* _ = char '\n' in
    return x_strs
end

let parse_input : string -> (X_str.t list, string) result =
  Angstrom.parse_string ~consume:All X_str.parse_all

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ x_strs = parse_input input in
    let code = x_strs |> List.map X_str.code_size |> List.fold_left ( + ) 0 in
    let memory =
      x_strs |> List.map X_str.memory_size |> List.fold_left ( + ) 0
    in
    Ok (string_of_int (code - memory))
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ x_strs = parse_input input in
    let code = x_strs |> List.map X_str.code_size |> List.fold_left ( + ) 0 in
    let encoded =
      x_strs |> List.map X_str.encoded_size |> List.fold_left ( + ) 0
    in
    Ok (string_of_int (encoded - code))
end
