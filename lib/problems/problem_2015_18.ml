open Import

let year = 2015
let day = 18

module Grid = struct
  include Table.Make (Int) (Int)

  type nonrec t = unit t

  let neighborhood : (int * int) list =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

  let neighbors ((x, y) : int * int) : (int * int) list =
    neighborhood
    |> List.map (fun (x', y') -> (x + x', y + y'))
    |> List.filter (fun (x, y) -> x >= 0 && x < 100 && y >= 0 && y < 100)

  let step_cell (t : t) (((x, y), on) : (int * int) * bool) : (int * int) * bool
      =
    let on_neighbors =
      neighbors (x, y) |> List.filter (fun k -> contains k t) |> List.length
    in
    ( (x, y),
      match on with
      | true -> on_neighbors = 2 || on_neighbors = 3
      | false -> on_neighbors = 3 )

  let step_cell_broken (t : t) (((x, y), on) : (int * int) * bool) :
      (int * int) * bool =
    match (x, y) with
    | 0, 0 -> ((x, y), true)
    | 0, 99 -> ((x, y), true)
    | 99, 0 -> ((x, y), true)
    | 99, 99 -> ((x, y), true)
    | _ -> (
        let on_neighbors =
          neighbors (x, y) |> List.filter (fun k -> contains k t) |> List.length
        in
        ( (x, y),
          match on with
          | true -> on_neighbors = 2 || on_neighbors = 3
          | false -> on_neighbors = 3 ))

  let step_with (step_fn : t -> (int * int) * bool -> (int * int) * bool)
      (t : t) : t =
    let cells_to_check =
      let ( let* ) x f = List.concat_map f x in
      let* cell = keys2 t in
      (cell, true) :: (List.map (fun k -> (k, contains k t)) @@ neighbors cell)
    in
    let new_cells =
      cells_to_check
      |> List.map (step_fn t)
      |> List.filter snd
      |> List.map (fun ((x, y), _) -> (x, y, ()))
    in
    of_entries new_cells

  let step = step_with step_cell
  let step_broken = step_with step_cell_broken

  let of_list (cs : char list list) : t =
    let entries =
      List.concat @@ List.concat
      @@ List.mapi
           (fun y line ->
             List.mapi (fun x c -> if c = '.' then [] else [ (x, y, ()) ]) line)
           cs
    in
    of_entries entries
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let grid =
      Grid.of_list @@ List.map String.to_list @@ String.split_on_char '\n' input
    in
    let grid = List.iterate 100 Grid.step grid in
    Ok (string_of_int @@ Grid.size grid)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let grid =
      input |> String.split_on_char '\n' |> List.map String.to_list
      |> Grid.of_list
      |> Grid.add (0, 0) ()
      |> Grid.add (0, 99) ()
      |> Grid.add (99, 0) ()
      |> Grid.add (99, 99) ()
    in
    let grid = List.iterate 100 Grid.step_broken grid in
    Ok (string_of_int @@ Grid.size grid)
end
