open Import

let year = 2015
let day = 9

module Graph = struct
  module Edge = struct
    type t = { from : string; to' : string; weight : int }

    let from (t : t) : string = t.from
    let to' (t : t) : string = t.to'

    let flip (t: t): t = {
      from = t.to';
      to' = t.from;
      weight = t.weight;
    }

    let parse : t Angstrom.t =
      let open Angstrom in
      let* from = String.of_chars <$> many1 ascii_letter in
      let* _ = string " to " in
      let* to' = String.of_chars <$> many1 ascii_letter in
      let* _ = string " = " in
      let* weight = unsigned_int in
      let* _ = char '\n' in
      return { from; to'; weight }
  end

  type t = { edges : Edge.t list; vertices : string list }

  let rec path_length (t : t) : string list -> int option = function
    | [] -> Some 0
    | [ _ ] -> Some 0
    | x :: y :: xs ->
        let- edge =
          List.find_opt (fun (e : Edge.t) -> e.from = x && e.to' = y) t.edges
        in
        let- rest_len = path_length t (y :: xs) in
        Some (edge.weight + rest_len)

  let shortest_path_length (t : t) : int option =
    let paths = List.permutations t.vertices in
    let path_lengths =
      List.concat_map Option.to_list @@ List.map (path_length t) paths
    in
    List.min_by Int.compare path_lengths

  let longest_path_length (t : t) : int option =
    let paths = List.permutations t.vertices in
    let path_lengths =
      List.concat_map Option.to_list @@ List.map (path_length t) paths
    in
    List.max_by Int.compare path_lengths

  let parse : t Angstrom.t =
    let open Angstrom in
    let* edges = many1 Edge.parse in
    let rev_edges = List.map Edge.flip edges in
    let edges = edges @ rev_edges in
    let vertices =
      List.sort_uniq String.compare
      @@ List.concat [ List.map Edge.from edges; List.map Edge.to' edges ]
    in
    return { edges; vertices }
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ graph = Angstrom.parse_string ~consume:All Graph.parse input in
    let shortest_path_length = Graph.shortest_path_length graph in
    match shortest_path_length with
    | None -> Error "No path found."
    | Some len -> Ok (string_of_int len)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let@ graph = Angstrom.parse_string ~consume:All Graph.parse input in
    let longest_path_length = Graph.longest_path_length graph in
    match longest_path_length with
    | None -> Error "No path found."
    | Some len -> Ok (string_of_int len)
end
