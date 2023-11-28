open Import

let year = 2015
let day = 7

module Cell = struct
  module Label : sig
    type t

    val equal : t -> t -> bool
    val of_string : string -> t
    val pp : t Fmt.t
    val compare : t -> t -> int
    val parse : t Angstrom.t
  end = struct
    type t = string

    let equal = String.equal
    let of_string x = x
    let pp : t Fmt.t = Fmt.string
    let compare = String.compare

    let parse : t Angstrom.t =
      let open Angstrom in
      take_while1 @@ function 'a' .. 'z' -> true | _ -> false
  end

  module Source = struct
    type t = Label of Label.t | Value of int

    let of_int (v : int) : t = Value v

    let parse_label : t Angstrom.t =
      let open Angstrom in
      let* label = Label.parse in
      return (Label label)

    let parse_value : t Angstrom.t =
      let open Angstrom in
      let* value = unsigned_int in
      return (Value value)

    let parse : t Angstrom.t =
      let open Angstrom in
      parse_label <|> parse_value
  end

  module Label_map = Map.Make (Label)

  module Content : sig
    type t =
      | Value of Source.t
      | Binary_operator of (Source.t * Source.t * (int -> int -> int))
      | Unary_operator of (Source.t * (int -> int))

    val parse : t Angstrom.t
  end = struct
    type t =
      | Value of Source.t
      | Binary_operator of (Source.t * Source.t * (int -> int -> int))
      | Unary_operator of (Source.t * (int -> int))

    module Ops = struct
      let and' a b = a land b land 0xffff
      let or' a b = a lor b land 0xffff
      let left a b = (a lsl b) land 0xffff
      let right a b = (a asr b) land 0xffff
      let not a = lnot a land 0xffff
    end

    let parse_value : t Angstrom.t =
      let open Angstrom in
      let* value = Source.parse in
      return @@ Value value

    let parse_binary : t Angstrom.t =
      let open Angstrom in
      let* x = Source.parse in
      let* op =
        string " AND " <|> string " OR " <|> string " LSHIFT "
        <|> string " RSHIFT "
      in
      let* y = Source.parse in
      match op with
      | " AND " -> return @@ Binary_operator (x, y, Ops.and')
      | " OR " -> return @@ Binary_operator (x, y, Ops.or')
      | " LSHIFT " -> return @@ Binary_operator (x, y, Ops.left)
      | " RSHIFT " -> return @@ Binary_operator (x, y, Ops.right)
      | x -> fail @@ Fmt.(str {|Unrecognized binary operation: %s|} x)

    let parse_not : t Angstrom.t =
      let open Angstrom in
      let* _ = string "NOT " in
      let* x = Source.parse in
      return @@ Unary_operator (x, Ops.not)

    let parse : t Angstrom.t =
      let open Angstrom in
      let* variant = parse_binary <|> parse_not <|> parse_value in
      let* _ = string " -> " in
      return variant
  end

  type t = { label : Label.t; content : Content.t }

  let parse : t Angstrom.t =
    let open Angstrom in
    let* content = Content.parse in
    let* label = Label.parse in
    return { label; content }

  let parse_all : t list Angstrom.t =
    let open Angstrom in
    let* values = sep_by1 (char '\n') parse in
    let* _ = char '\n' in
    return values

  let rec evaluate (results_by_label : int Label_map.t)
      (cells_by_label : t Label_map.t) (cell : t) :
      (int Label_map.t * int, string) result =
    (* Return the result if it's cached. *)
    match Label_map.find_opt cell.label results_by_label with
    | Some value -> Ok (results_by_label, value)
    | None -> (
        (* If it isn't cached, evaluate each expression. *)
        match cell with
        | { label; content = Value x } ->
            let@ results_by_label, x =
              match x with
              | Value x -> Ok (results_by_label, x)
              | Label x -> (
                  match Label_map.find_opt x cells_by_label with
                  | None ->
                      Error Fmt.(str "Cell not found: label = %a" Label.pp x)
                  | Some x -> evaluate results_by_label cells_by_label x)
            in
            let results_by_label = Label_map.add label x results_by_label in
            Ok (results_by_label, x)
        | { label; content = Unary_operator (x, op) } -> (
            match x with
            | Value x ->
                let n = op x in
                let results_by_label = Label_map.add label n results_by_label in
                Ok (results_by_label, n)
            | Label x -> (
                match Label_map.find_opt x cells_by_label with
                | None ->
                    Error Fmt.(str "Cell not found: label = %a" Label.pp x)
                | Some cell ->
                    let@ results_by_label, value =
                      evaluate results_by_label cells_by_label cell
                    in
                    let n = op value in
                    let results_by_label =
                      Label_map.add label n results_by_label
                    in
                    Ok (results_by_label, n)))
        | { label; content = Binary_operator (x, y, op) } ->
            let@ results_by_label, x =
              match x with
              | Value x -> Ok (results_by_label, x)
              | Label x -> (
                  match Label_map.find_opt x cells_by_label with
                  | None ->
                      Error Fmt.(str "Cell not found: label = %a" Label.pp x)
                  | Some x -> evaluate results_by_label cells_by_label x)
            in
            let@ results_by_label, y =
              match y with
              | Value y -> Ok (results_by_label, y)
              | Label y -> (
                  match Label_map.find_opt y cells_by_label with
                  | None ->
                      Error Fmt.(str "Cell not found: label = %a" Label.pp y)
                  | Some y -> evaluate results_by_label cells_by_label y)
            in
            let n = op x y in
            let results_by_label = Label_map.add label n results_by_label in
            Ok (results_by_label, n))

  let evaulate_label (cells : t list) (label : Label.t) : (int, string) result =
    let cells_by_label =
      Label_map.add_seq
        (List.to_seq (List.map (fun c -> (c.label, c)) cells))
        Label_map.empty
    in
    match Label_map.find_opt label cells_by_label with
    | None -> Error (Fmt.str "Cell not found: label = %a" Label.pp label)
    | Some cell ->
        let@ _, result = evaluate Label_map.empty cells_by_label cell in
        Ok result
end

let parse_input (input : string) : (Cell.t list, string) result =
  let open Angstrom in
  parse_string ~consume:All Cell.parse_all input

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ cells = parse_input input in
    let@ result = Cell.evaulate_label cells (Cell.Label.of_string "a") in
    Ok (string_of_int result)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ cells = parse_input input in
    let@ result = Cell.evaulate_label cells (Cell.Label.of_string "a") in
    let cells =
      cells
      |> List.map @@ fun (c : Cell.t) ->
         if Cell.Label.(equal c.label (of_string "b")) then
           Cell.
             {
               label = Cell.Label.of_string "b";
               content = Value (Cell.Source.of_int result);
             }
         else c
    in
    let@ result = Cell.evaulate_label cells (Cell.Label.of_string "a") in
    Ok (string_of_int result)
end
