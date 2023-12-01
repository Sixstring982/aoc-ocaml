open Import

let year = 2015
let day = 11

module Password = struct
  type t = int list

  let of_string (s : string) : (t, string) result =
    if String.length s <> 8 then Error "Passwords must be 8 characters."
    else
      Ok
        (List.map (fun c -> int_of_char c - int_of_char 'a') @@ String.to_list s)

  let to_string (t : t) : string =
    String.of_chars @@ List.map (fun i -> char_of_int (i + int_of_char 'a')) t

  let rec has_increasing_straight : t -> bool = function
    | x :: y :: z :: xs ->
        if x = y - 1 && y = z - 1 then true
        else (has_increasing_straight [@tailrec]) (y :: z :: xs)
    | _ -> false

  let is_letter_ambiguous : int -> bool =
    let ambiguous_letters =
      List.map (fun c -> int_of_char c - int_of_char 'a') [ 'i'; 'o'; 'l' ]
    in
    fun c -> List.mem c ambiguous_letters

  let contains_two_pairs : t -> bool =
    let rec go (prev_was_pair : bool) (found_prev_pair : bool) :
        int list -> bool = function
      | x :: y :: xs ->
          if x <> y || prev_was_pair then
            (go [@tailcall]) false found_prev_pair (y :: xs)
          else if found_prev_pair then true
          else (go [@tailcall]) true true (y :: xs)
      | _ -> false
    in
    go false false

  let is_valid (t : t) : bool =
    has_increasing_straight t && contains_two_pairs t

  let rec increment (t : t) : (t, string) result =
    match t with
    | [] -> Error "Password too short"
    | [ x ] -> (
        match x with
        | 25 -> Ok [ 0 ]
        | x ->
            let x = if is_letter_ambiguous (x + 1) then x + 2 else x + 1 in
            Ok [ x ])
    | x :: xs ->
        if is_letter_ambiguous x then
          Ok ((x + 1) :: List.replicate 0 (List.length xs))
        else
          let@ y, xs =
            let@ inc = increment xs in
            Ok (Option.get @@ List.hd inc, List.tl inc)
          in
          if List.for_all (( = ) 0) (y :: xs) then
            match x with
            | 25 -> Ok (0 :: y :: xs)
            | x ->
                let x = if is_letter_ambiguous (x + 1) then x + 2 else x + 1 in
                Ok (x :: y :: xs)
          else Ok (x :: y :: xs)
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let input = String.trim input in
    let@ password = Password.of_string input in
    let@ password = Password.increment password in
    let@ password =
      let module List = List.Monad2 (Result.Monad2) in
      List.iterate_until Password.is_valid Password.increment password
    in
    Ok (Password.to_string password)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let input = String.trim input in
    let@ password = Password.of_string input in
    let@ password = Password.increment password in
    let@ password =
      let module List = List.Monad2 (Result.Monad2) in
      List.iterate_until Password.is_valid Password.increment password
    in
    let@ password = Password.increment password in
    let@ password =
      let module List = List.Monad2 (Result.Monad2) in
      List.iterate_until Password.is_valid Password.increment password
    in
    Ok (Password.to_string password)
end
