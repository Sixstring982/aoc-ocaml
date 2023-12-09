open Import

let year = 2023
let day = 9

let rec differences: int list -> int list = function
  | [] -> []
  | [_] -> []
  | x :: y :: xs ->
      (y - x) :: differences (y :: xs)

let rec extrapolate (xs: int list): int =
  let ds = differences xs in
  if List.for_all ~f:((=) 0) ds then Option.value_exn @@ List.hd xs
  else
    let extrapolated = extrapolate ds in
    List.last_exn xs + extrapolated

let rec extrapolate_backwards (xs: int list): int =
  let ds = differences xs in
  if List.for_all ~f:((=) 0) ds then Option.value_exn @@ List.hd xs
  else
    let extrapolated = extrapolate_backwards ds in
    List.hd_exn xs - extrapolated

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let histories =
      input
      |> String.split ~on:'\n'
      |> List.map ~f:(String.split ~on:' ')
      |> List.map ~f:(List.filter ~f:(not << String.is_empty))
      |> List.map ~f:(List.map ~f:int_of_string)
      |> List.filter ~f:(not << List.is_empty)
    in
    let extrapolated_values = List.map ~f:extrapolate histories in
    Ok (string_of_int @@ Int.sum extrapolated_values)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let histories =
      input
      |> String.split ~on:'\n'
      |> List.map ~f:(String.split ~on:' ')
      |> List.map ~f:(List.filter ~f:(not << String.is_empty))
      |> List.map ~f:(List.map ~f:int_of_string)
      |> List.filter ~f:(not << List.is_empty)
    in
    let extrapolated_values = List.map ~f:extrapolate_backwards histories in
    Ok (string_of_int @@ Int.sum extrapolated_values)
end
