include Let
include Fn

type void = |

let comparing : ('a -> 'a -> int) -> ('b -> 'a) -> 'b -> 'b -> int =
 fun compare_a a_of_b -> Fn.on compare_a a_of_b

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
  let product : int list -> int = List.fold_left ( * ) 1
  let maximum = List.fold_left max min_int
  let minimum = List.fold_left min max_int

  let remove_first_by (f : int list -> int) (xs : int list) : int list =
    let v = f xs in
    let rec go (acc : int list) = function
      | [] -> List.rev acc
      | x :: xs ->
          if x = v then List.rev_append acc xs
          else (go [@tailcall]) (x :: acc) xs
    in
    go [] xs
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

module Multimap = struct
  module type KEY = sig
    include Map.OrderedType
  end

  module type T = sig
    module Key : KEY
    module Map : module type of Map.Make (Key) with type key = Key.t

    type 'v t

    (* Constructors *)

    val empty : 'v t
    val group_by : ('v -> Key.t) -> 'v list -> 'v t
    val of_entries : (Key.t * 'v) list -> 'v t

    (* Mutators *)

    val map : ('a -> 'b) -> 'a t -> 'b t

    (* Accessors *)
    end

  module Make (Key : KEY) : T with type Key.t = Key.t = struct
    module Key = Key
    module Map = Map.Make (Key)

    type 'v t = 'v list Map.t

    let empty = Map.empty
    let map (f : 'a -> 'b) : 'a t -> 'b t = Map.map @@ List.map f

    let group_by (key_fn : 'v -> Key.t) (vs : 'v list) : 'v t =
      vs
      |> List.fold_left
           (fun map next ->
             Map.update (key_fn next)
               (function None -> Some [ next ] | Some vs -> Some (next :: vs))
               map)
           empty
      |> Map.map List.rev

    let of_entries (es : (Key.t * 'v) list) : 'v t = map snd @@ group_by fst es
  end
  end

module Table = struct
  module type KEY = sig
    include Map.OrderedType
  end

  module type T = sig
    module K1 : KEY
    module K2 : KEY

    type 'v t

    (* Constructors *)

    val empty : 'v t
    val group_by : ('v -> K1.t) -> ('v -> K2.t) -> 'v list -> 'v t
    val of_entries : (K1.t * K2.t * 'v) list -> 'v t

    (* Mutators *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    val add : K1.t * K2.t -> 'v -> 'v t -> 'v t
    val remove : K1.t * K2.t -> 'v t -> 'v t
    val update : K1.t * K2.t -> ('v option -> 'v option) -> 'v t -> 'v t

    (* Accessors *)
    val keys : 'a t -> K2.t list
    val keys2 : 'a t -> (K1.t * K2.t) list
    val size : 'a t -> int
    val contains : K1.t * K2.t -> 'v t -> bool
    val find_opt : K1.t -> K2.t -> 'v t -> 'v option
  end

  module Make (K1 : KEY) (K2 : KEY) :
    T with type K1.t = K1.t and type K2.t = K2.t = struct
    module K1 = K1
    module K2 = K2
    module Map1 = Map.Make (K1)
    module Map2 = Map.Make (K2)

    type 'v t = 'v Map1.t Map2.t

    let empty = Map2.empty
    let map (f : 'a -> 'b) : 'a t -> 'b t = Map2.map (Map1.map f)
    let keys (t : 'a t) : K2.t list = List.map fst @@ Map2.bindings t

    let keys2 (t : 'a t) : (K1.t * K2.t) list =
      let keys2 = keys t in
      List.concat_map
        (fun k2 ->
          let m1 = Map2.find k2 t in
          let k1s = List.map fst @@ Map1.bindings m1 in
          List.map (fun k1 -> (k1, k2)) k1s)
        keys2

    let size (m : 'v t) : int = List.length @@ keys2 m

    let contains ((k1, k2) : K1.t * K2.t) (m2 : 'v t) : bool =
      match Map2.find_opt k2 m2 with None -> false | Some m1 -> Map1.mem k1 m1

    let find_opt : K1.t -> K2.t -> 'v t -> 'v option =
     fun k1 k2 m2 ->
      let- m1 = Map2.find_opt k2 m2 in
      Map1.find_opt k1 m1

    let add ((k1, k2) : K1.t * K2.t) (v : 'v) (m : 'v t) : 'v t =
      Map2.update k2
        (function
          | None -> Some (Map1.singleton k1 v)
          | Some m1 -> Some (Map1.add k1 v m1))
        m

    let remove ((k1, k2) : K1.t * K2.t) (m : 'v t) : 'v t =
      Map2.update k2
        (function None -> None | Some m1 -> Some (Map1.remove k1 m1))
        m

    let update ((k1, k2) : K1.t * K2.t) (f : 'v option -> 'v option) (m : 'v t)
        : 'v t =
      match f @@ find_opt k1 k2 m with
      | None -> remove (k1, k2) m
      | Some v -> add (k1, k2) v m

    let group_by (k1_fn : 'v -> K1.t) (k2_fn : 'v -> K2.t) (vs : 'v list) : 'v t
        =
      List.fold_left
        (fun m2 next ->
          let k1, k2 = (k1_fn next, k2_fn next) in
          Map2.update k2
            (function
              | None -> Some (Map1.singleton k1 next)
              | Some m1 -> Some (Map1.add k1 next m1))
            m2)
        empty vs

    let of_entries (es : (K1.t * K2.t * 'v) list) : 'v t =
      List.fold_left
        (fun m2 (k1, k2, v) ->
          Map2.update k2
            (function
              | None -> Some (Map1.singleton k1 v)
              | Some m1 -> Some (Map1.add k1 v m1))
            m2)
        empty es
  end
end

module String = struct
  include String

  let of_chars : char list -> string =
    Buffer.contents << Buffer.of_seq << List.to_seq

  let to_list : string -> char list = List.of_seq << to_seq
end

module List = struct
  include List

  let ( let* ) x f = List.concat_map f x

  let rec remove_prefix :
      ('a -> 'a -> bool) -> prefix:'a list -> 'a list -> 'a list option =
   fun eq ~prefix xs ->
    match (prefix, xs) with
    | [], xs -> Some xs
    | _, [] -> None
    | p :: ps, x :: xs ->
        if eq x p then (remove_prefix [@tailcall]) eq ~prefix:ps xs else None

  let iota (n : int) : int list =
    let rec go (acc : int list) = function
      | x when x = n -> acc
      | n -> (go [@tailcall]) (n :: acc) (n + 1)
    in
    go [] 0

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

  let rec iterate (n : int) (f : 'a -> 'a) (a : 'a) : 'a =
    if n <= 0 then a else (iterate [@tailcall]) (n - 1) f (f a)

  let iterate_i (n : int) (f : int -> 'a -> 'a) (a : 'a) : 'a =
    let rec go (a : 'a) = function
      | n' when n' = n -> a
      | n -> (go [@tailcall]) (f n a) (n + 1)
    in
    go a 0

  let hd : 'x list -> 'x option = function [] -> None | x :: _ -> Some x

  let rec last : 'x list -> 'x option = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: xs -> (last [@tailcall]) xs

  let rec initial : 'x list -> 'x list = function
    | [] -> []
    | [ _ ] -> []
    | x :: xs -> x :: initial xs

  let rec take (n : int) : 'x list -> 'x list = function
    | [] -> []
    | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs

  let rec drop (n : int) : 'x list -> 'x list = function
    | [] -> []
    | x :: xs -> if n <= 0 then x :: xs else (drop [@tailcall]) (n - 1) xs

  let ends_with (eq : 'a -> 'a -> bool) ~(suffix : 'a list) (xs : 'a list) :
      bool =
    let xs = drop (List.length xs - List.length suffix) xs in
    equal eq suffix xs

  let replicate (n : int) (a : 'a) : 'a list =
    let rec go (acc : 'a list) = function
      | n when n <= 0 -> acc
      | n -> (go [@tailcall]) (a :: acc) (n - 1)
    in
    go [] n

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

  let negative_int : int t =
    let* _ = char '-' in
    let* n = unsigned_int in
    return (-n)

  let signed_int : int t = unsigned_int <|> negative_int

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

module Option = struct
  include Option

  let ( <|> ) a b = match a with None -> b | Some a -> Some a
end
