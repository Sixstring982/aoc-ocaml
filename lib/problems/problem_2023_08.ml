[@@@warning "-69"]

open Import

let year = 2023
let day = 8

module Instruction = struct
  type t = Left | Right
end

module Instructions = struct
  type t = Instruction.t list
end

module Node = struct
  type t = { label : string; left_label : string; right_label : string }
end

module Graph = struct
  module Map = Map.Make_tree (String)

  type t = Node.t Map.t
end

module Document = struct
  type t = { instructions : Instructions.t; graph : Graph.t }

  let follow_instructions (t : t) : int =
    let rec go (acc : int) (node : Node.t) : Instructions.t -> int = function
      | [] -> (go [@tailcall]) acc node t.instructions
      | x :: xs -> (
          if String.equal node.label "ZZZ" then acc
          else
            let new_label =
              match x with Left -> node.left_label | Right -> node.right_label
            in
            match Graph.Map.find t.graph new_label with
            | None ->
                failwith @@ Fmt.str "Label not found in graph: %s" new_label
            | Some node -> (go [@tailcall]) (acc + 1) node xs)
    in
    match Graph.Map.find t.graph "AAA" with
    | None -> failwith @@ "Label not found in graph: AAA"
    | Some node -> go 0 node t.instructions

  let step_node (t : t) (instr : Instruction.t) (node : Node.t) : Node.t =
    let new_label =
      match instr with Left -> node.left_label | Right -> node.right_label
    in
    match Graph.Map.find t.graph new_label with
    | None -> failwith @@ Fmt.str "Label not found in graph: %s" new_label
    | Some node -> node

  let ghost_follow_instructions (t : t) : int =
    let a_nodes =
      t.graph |> Graph.Map.to_alist
      |> List.filter ~f:(String.is_suffix ~suffix:"A" << fst)
      |> List.map ~f:snd
    in
    let rec go (acc : int) (nodes : Node.t list) : Instructions.t -> int =
      function
      | [] -> (go [@tailcall]) acc nodes t.instructions
      | x :: xs ->
          if
            List.for_all
              ~f:(fun (x : Node.t) -> String.is_suffix ~suffix:"Z" x.label)
              nodes
          then acc
          else
            let nodes = List.map ~f:(step_node t x) nodes in
            (go [@tailcall]) (acc + 1) nodes xs
    in
    go 0 a_nodes t.instructions
end

module Angstrom = (* {{{ *) struct
  include Angstrom

  let instruction : Instruction.t t =
    let* c = char 'L' <|> char 'R' in
    match c with
    | 'L' -> return Instruction.Left
    | 'R' -> return Instruction.Right
    | c -> fail @@ Fmt.str "Invalid instruction: %c" c

  let instructions : Instructions.t t =
    let* cs = many1 instruction in
    let* _ = char '\n' in
    return cs

  let node : Node.t t =
    let parse_label = String.of_list <$> count 3 uppercase_ascii in
    let* label = parse_label in
    let* _ = string " = (" in
    let* left_label = parse_label in
    let* _ = string ", " in
    let* right_label = parse_label in
    let* _ = string ")\n" in
    return Node.{ label; left_label; right_label }

  let graph : Graph.t t =
    let* nodes = many1 node in
    let graph =
      nodes
      |> List.map ~f:(fun (x : Node.t) -> (x.label, x))
      |> Graph.Map.of_alist
    in
    match graph with
    | `Duplicate_key s -> fail ("Duplicate node label: " ^ s)
    | `Ok x -> return x

  let document : Document.t t =
    let* instructions = instructions in
    let* _ = char '\n' in
    let* graph = graph in
    return Document.{ instructions; graph }
end
(* }}} *)

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ document = Angstrom.(parse_string ~consume:All document input) in
    Ok (string_of_int @@ Document.follow_instructions document)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ document = Angstrom.(parse_string ~consume:All document input) in
    Ok (string_of_int @@ Document.ghost_follow_instructions document)
end
