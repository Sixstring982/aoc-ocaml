open Import

let year = 2023
let day = 3

module Table = struct
  module Table = Table.Make (Int) (Int)

  type t = char Table.t

  let neighborhood : (int * int) list =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

  let neighbors (t : t) ((x, y) : int * int) : (int * int * char) list =
    List.filter_map neighborhood ~f:(fun (x', y') ->
        let x, y = (x + x', y + y') in
        match Table.find_opt x y t with None -> None | Some c -> Some (x, y, c))

  let find_symbols : t -> (int * int * char) list =
    Table.find_all ~f:(not << Char.is_digit)

  let rec seek_to_start_of_part_number (t : t) ((x, y) : int * int) : int * int
      =
    match Table.find_opt (x - 1) y t with
    | None -> (x, y)
    | Some c ->
        if Char.is_digit c then seek_to_start_of_part_number t (x - 1, y)
        else (x, y)

  let scan_part_number (t : t) ((x, y) : int * int) : int =
    let rec go (acc : int) (x, y) : int =
      match Table.find_opt x y t with
      | None -> acc
      | Some c ->
          if not @@ Char.is_digit c then acc
          else go ((acc * 10) + (int_of_char c - int_of_char '0')) (x + 1, y)
    in
    go 0 (x, y)

  let parse_part_number_at (t : t) ((x, y) : int * int) : int =
    let x, y = seek_to_start_of_part_number t (x, y) in
    scan_part_number t (x, y)

  let part_numbers_around_symbol (t : t) ((x, y) : int * int) : int list =
    neighbors t (x, y)
    |> List.filter_map ~f:(fun (x, y, c) ->
           if Char.is_digit c then Some (x, y) else None)
    |> List.map ~f:(parse_part_number_at t)
    |> List.dedup_and_sort ~compare:Int.compare

  module Gear = struct
    type t = { ratio : int }

    let ratio (t : t) : int = t.ratio
  end

  let find_gears (t : t) : Gear.t list =
    find_symbols t
    |> List.filter_map ~f:(fun (x, y, c) ->
           if not @@ Char.equal c '*' then None
           else
             let part_numbers = part_numbers_around_symbol t (x, y) in
             match part_numbers with
             | [ r1; r2 ] -> Some Gear.{ ratio = r1 * r2 }
             | _ -> None)

  let of_input (input : string) : char Table.t =
    let entries =
      let ( let* ) x f = List.concat_map ~f x in
      let* y, line =
        let ls = String.split ~on:'\n' input in
        match
          List.zip
            (List.range ~start:`inclusive ~stop:`exclusive 0 (List.length ls))
            ls
        with
        | Unequal_lengths -> failwith "Unreachable"
        | Ok ls -> ls
      in
      let* x, c =
        let ls = String.to_list line in
        match
          List.zip
            (List.range ~start:`inclusive ~stop:`exclusive 0 (List.length ls))
            ls
        with
        | Unequal_lengths -> failwith "Unreachable"
        | Ok ls -> ls
      in
      if Char.equal c '.' then [] else [ (x, y, c) ]
    in
    Table.of_entries entries
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let table = Table.of_input input in
    let all_part_numbers =
      let ( let* ) x f = List.concat_map ~f x in
      let* x, y, _ = Table.find_symbols table in
      Table.part_numbers_around_symbol table (x, y)
    in
    Ok (string_of_int @@ Int.sum all_part_numbers)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let table = Table.of_input input in
    let gears = Table.find_gears table in
    let ratios = List.map gears ~f:Table.Gear.ratio in
    Ok (string_of_int @@ Int.sum ratios)
end
