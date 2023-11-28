include Let
include Fn

module Monad2 = struct
  module type I = sig
    type ('a, 'b) t

    val return : 'a -> ('a, 'b) t
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  end

  module type T = sig
    include I

    val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val sequence : ('a, 'b) t list -> ('a list, 'b) t
  end

  module Make (I : I) : T with type ('a, 'b) t = ('a, 'b) I.t = struct
    include I

    let ( let* ) = I.bind

    let rec sequence : ('a, 'b) t list -> ('a list, 'b) t = function
      | [] -> return []
      | x :: xs ->
          let* x = x in
          let* xs = sequence xs in
          return @@ (x :: xs)
  end
end

module Int = struct
  include Int

  let sum : int list -> int = List.fold_left ( + ) 0
end

module Float = struct
  include Float

  let sum : float list -> float = List.fold_left ( +. ) 0.
end

module Result = struct
  include Result

  module Monad2 : Monad2.T with type ('a, 'b) t = ('a, 'b) result =
  Monad2.Make (struct
    type ('a, 'b) t = ('a, 'b) result

    let return = ok
    let bind = bind
  end)
end

module String = struct
  include String

  let of_chars : char list -> string =
    Buffer.contents << Buffer.of_seq << List.to_seq

  let to_list : string -> char list = List.of_seq << to_seq
end

module List = struct
  include List

  let rec permutations : 'a list -> 'a list list =
    let ins_all_positions x l =
      let rec aux prev acc = function
        | [] -> (prev @ [ x ]) :: acc |> List.rev
        | hd :: tl as l -> aux (prev @ [ hd ]) ((prev @ [ x ] @ l) :: acc) tl
      in
      aux [] [] l
    in
    function
    | [] -> []
    | x :: [] -> [ [ x ] ]
    | x :: xs ->
        List.fold_left
          (fun acc p -> acc @ ins_all_positions x p)
          [] (permutations xs)

  let min_by : ('a -> 'a -> int) -> 'a list -> 'a option =
   fun compare xs ->
    List.fold_left
      (fun (acc : 'a option) (next : 'a) ->
        match acc with
        | None -> Some next
        | Some min -> if compare min next < 0 then Some min else Some next)
      None xs

  let max_by : ('a -> 'a -> int) -> 'a list -> 'a option =
   fun compare xs ->
    List.fold_left
      (fun (acc : 'a option) (next : 'a) ->
        match acc with
        | None -> Some next
        | Some min -> if compare min next > 0 then Some min else Some next)
      None xs

  let replicate (a : 'a) : int -> 'a list =
    let rec go (acc : 'a list) : int -> 'a list = function
      | 0 -> acc
      | n -> (go [@tailcall]) (a :: acc) (n - 1)
    in
    go []

  let rec iterate (n : int) (f : 'a -> 'a) (a : 'a) : 'a =
    if n <= 0 then a else (iterate [@tailcall]) (n - 1) f (f a)

  module Monad2 (M : Monad2.T) = struct
    include List

    let rec iterate_until (pred : 'a -> bool) (f : 'a -> ('a, 'm2) M.t) (a : 'a)
        : ('a, 'm2) M.t =
      let open M in
      if pred a then return a
      else
        let* a = f a in
        (iterate_until [@tailcall]) pred f a
  end
end

module Angstrom = struct
  include Angstrom

  let unsigned_int : int t =
    int_of_string <$> take_while1 @@ function '0' .. '9' -> true | _ -> false

  let lowercase_ascii : char t =
    choice @@ List.init 26 (fun x -> char @@ char_of_int (x + int_of_char 'a'))

  let uppercase_ascii : char t =
    choice @@ List.init 26 (fun x -> char @@ char_of_int (x + int_of_char 'A'))

  let ascii_letter : char t = uppercase_ascii <|> lowercase_ascii

  let lowercase_hex : char t =
    choice (List.init 6 (fun x -> char @@ char_of_int (x + int_of_char 'a')))
    <|> choice
          (List.init 10 (fun x -> char @@ char_of_int (x + int_of_char '0')))

  let hex_nibble : int t =
    let* c = lowercase_hex in
    match c with
    | '0' .. '9' as c -> return @@ (int_of_char c - int_of_char '0')
    | 'a' .. 'f' as c -> return @@ (int_of_char c - int_of_char 'a' + 10)
    | c -> fail @@ Fmt.str "Invalid hex nibble: '%c'" c

  let hex_byte : int t =
    let* high = hex_nibble in
    let* low = hex_nibble in
    return @@ ((high lsl 4) lor low)
end
