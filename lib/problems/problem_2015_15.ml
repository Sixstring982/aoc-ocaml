open Import

let year = 2015
let day = 15

module Ingredient = struct
  type t = { capacity : int; durability : int; flavor : int; texture : int; calories: int  }

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _ = many1 ascii_letter in
    let* _ = string ": capacity " in
    let* capacity = signed_int in
    let* _ = string ", durability " in
    let* durability = signed_int in
    let* _ = string ", flavor " in
    let* flavor = signed_int in
    let* _ = string ", texture " in
    let* texture = signed_int in
    let* _ = string ", calories " in
    let* calories = signed_int in
    let* _ = char '\n' in
    return { capacity; durability; flavor; texture; calories }

  let parse_all : t list Angstrom.t = Angstrom.many1 parse
end

let distributions2 (n : int) : (int * int) list =
  let ( let* ) x f = List.concat_map f x in
  let* x = List.iota n in
  let* y = List.iota n in
  if x + y = n then [ (x, y) ] else []

let distributions4 (n : int) : (int * int * int * int) list =
  let ( let* ) x f = List.concat_map f x in
  let* w = List.iota n in
  let* x = List.iota n in
  let* y = List.iota n in
  let* z = List.iota n in
  if w + x + y + z = n then [ (w, x, y, z) ] else []

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ ingredients =
      Angstrom.parse_string ~consume:All Ingredient.parse_all input
    in
    match ingredients with
    | [ n1; n2 ] -> (
        let eval (x, y) =
          Int.product
            [
              max 0 @@ Int.sum @@ [ x * n1.capacity; y * n2.capacity ];
              max 0 @@ Int.sum [ x * n1.durability; y * n2.durability ];
              max 0 @@ Int.sum [ x * n1.flavor; y * n2.flavor ];
              max 0 @@ Int.sum [ x * n1.texture; y * n2.texture ];
            ]
        in
        let ds = distributions2 100 in
        let m = List.max_by (comparing Int.compare eval) ds in
        match m with
        | None -> Error "Not enough distributions"
        | Some d -> Ok (string_of_int @@ eval d))
    | [ n1; n2; n3; n4 ] -> (
        let eval (w, x, y, z) =
          Int.product
            [
              max 0
              @@ Int.sum
                   [
                     w * n1.capacity;
                     x * n2.capacity;
                     y * n3.capacity;
                     z * n4.capacity;
                   ];
              max 0
              @@ Int.sum
                   [
                     w * n1.durability;
                     x * n2.durability;
                     y * n3.durability;
                     z * n4.durability;
                   ];
              max 0
              @@ Int.sum
                   [
                     w * n1.flavor; x * n2.flavor; y * n3.flavor; z * n4.flavor;
                   ];
              max 0
              @@ Int.sum
                   [
                     w * n1.texture;
                     x * n2.texture;
                     y * n3.texture;
                     z * n4.texture;
                   ];
            ]
        in
        let ds = distributions4 100 in
        let m = List.max_by (comparing Int.compare eval) ds in
        match m with
        | None -> Error "Not enough distributions"
        | Some d -> Ok (string_of_int @@ eval d))
    | _ ->
        Error
          (Fmt.str "Solution not implemented for ingredients of length %d"
          @@ List.length ingredients)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ ingredients =
      Angstrom.parse_string ~consume:All Ingredient.parse_all input
    in
    match ingredients with
    | [ n1; n2 ] -> (
        let eval (x, y) =
          let calories = x * n1.calories + y * n2.calories in
          if calories <> 500 then 0 else
          Int.product
            [
              max 0 @@ Int.sum @@ [ x * n1.capacity; y * n2.capacity ];
              max 0 @@ Int.sum [ x * n1.durability; y * n2.durability ];
              max 0 @@ Int.sum [ x * n1.flavor; y * n2.flavor ];
              max 0 @@ Int.sum [ x * n1.texture; y * n2.texture ];
            ]
        in
        let ds = distributions2 100 in
        let m = List.max_by (comparing Int.compare eval) ds in
        match m with
        | None -> Error "Not enough distributions"
        | Some d -> Ok (string_of_int @@ eval d))
    | [ n1; n2; n3; n4 ] -> (
        let eval (w, x, y, z) =
          let calories = w * n1.calories + x * n2.calories + y * n3.calories + z * n4.calories in
          if calories <> 500 then 0 else
          Int.product
            [
              max 0
              @@ Int.sum
                   [
                     w * n1.capacity;
                     x * n2.capacity;
                     y * n3.capacity;
                     z * n4.capacity;
                   ];
              max 0
              @@ Int.sum
                   [
                     w * n1.durability;
                     x * n2.durability;
                     y * n3.durability;
                     z * n4.durability;
                   ];
              max 0
              @@ Int.sum
                   [
                     w * n1.flavor; x * n2.flavor; y * n3.flavor; z * n4.flavor;
                   ];
              max 0
              @@ Int.sum
                   [
                     w * n1.texture;
                     x * n2.texture;
                     y * n3.texture;
                     z * n4.texture;
                   ];
            ]
        in
        let ds = distributions4 100 in
        let m = List.max_by (comparing Int.compare eval) ds in
        match m with
        | None -> Error "Not enough distributions"
        | Some d -> Ok (string_of_int @@ eval d))
    | _ ->
        Error
          (Fmt.str "Solution not implemented for ingredients of length %d"
          @@ List.length ingredients)
end
