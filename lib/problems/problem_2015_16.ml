open Import

let year = 2015
let day = 16

module Sue = struct
  module Properties = struct
    type t = {
      children : int option;
      cats : int option;
      samoyeds : int option;
      pomeranians : int option;
      akitas : int option;
      vizslas : int option;
      goldfish : int option;
      trees : int option;
      cars : int option;
      perfumes : int option;
    }

    let make ?(children : int option) ?(cats : int option)
        ?(samoyeds : int option) ?(pomeranians : int option)
        ?(akitas : int option) ?(vizslas : int option) ?(goldfish : int option)
        ?(trees : int option) ?(cars : int option) ?(perfumes : int option) () :
        t =
      {
        children;
        cats;
        samoyeds;
        pomeranians;
        akitas;
        vizslas;
        goldfish;
        trees;
        cars;
        perfumes;
      }

    let empty = make ()

    let merge (a : t) (b : t) : t =
      let open Option in
      {
        children = a.children <|> b.children;
        cats = a.cats <|> b.cats;
        samoyeds = a.samoyeds <|> b.samoyeds;
        pomeranians = a.pomeranians <|> b.pomeranians;
        akitas = a.akitas <|> b.akitas;
        vizslas = a.vizslas <|> b.vizslas;
        goldfish = a.goldfish <|> b.goldfish;
        trees = a.trees <|> b.trees;
        cars = a.cars <|> b.cars;
        perfumes = a.perfumes <|> b.perfumes;
      }

    let merge_all = List.fold_left merge empty

    let parse : t Angstrom.t =
      let open Angstrom in
      let* name =
        choice
        @@ List.map string
             [
               "children";
               "cats";
               "samoyeds";
               "pomeranians";
               "akitas";
               "vizslas";
               "goldfish";
               "trees";
               "cars";
               "perfumes";
             ]
      in
      let* _ = string ": " in
      let* value = unsigned_int in
      match name with
      | "children" -> return @@ make ~children:value ()
      | "cats" -> return @@ make ~cats:value ()
      | "samoyeds" -> return @@ make ~samoyeds:value ()
      | "pomeranians" -> return @@ make ~pomeranians:value ()
      | "akitas" -> return @@ make ~akitas:value ()
      | "vizslas" -> return @@ make ~vizslas:value ()
      | "goldfish" -> return @@ make ~goldfish:value ()
      | "trees" -> return @@ make ~trees:value ()
      | "cars" -> return @@ make ~cars:value ()
      | "perfumes" -> return @@ make ~perfumes:value ()
      | s -> fail @@ "Unrecognized property: " ^ s

    let template : t =
      {
        children = Some 3;
        cats = Some 7;
        samoyeds = Some 2;
        pomeranians = Some 3;
        akitas = Some 0;
        vizslas = Some 0;
        goldfish = Some 5;
        trees = Some 3;
        cars = Some 2;
        perfumes = Some 1;
      }

    let is_subset_of_template (t: t) = 
      let of' = template in
      List.for_all id
        [
          t.children = None || t.children = of'.children;
          t.cats = None || t.cats = of'.cats;
          t.samoyeds = None || t.samoyeds = of'.samoyeds;
          t.pomeranians = None || t.pomeranians = of'.pomeranians;
          t.akitas = None || t.akitas = of'.akitas;
          t.vizslas = None || t.vizslas = of'.vizslas;
          t.goldfish = None || t.goldfish = of'.goldfish;
          t.trees = None || t.trees = of'.trees;
          t.cars = None || t.cars = of'.cars;
          t.perfumes = None || t.perfumes = of'.perfumes;
        ]
    let is_encabulated_subset_of_template (t : t) : bool =
      let of' = template in
      List.for_all id
        [
          t.children = None || t.children = of'.children;
          t.cats = None || (Option.get t.cats) > (Option.get of'.cats);
          t.samoyeds = None || t.samoyeds = of'.samoyeds;
          t.pomeranians = None || (Option.get t.pomeranians) < (Option.get of'.pomeranians);
          t.akitas = None || t.akitas = of'.akitas;
          t.vizslas = None || t.vizslas = of'.vizslas;
          t.goldfish = None || (Option.get t.goldfish) < (Option.get of'.goldfish);
          t.trees = None || (Option.get t.trees) > (Option.get of'.trees);
          t.cars = None || t.cars = of'.cars;
          t.perfumes = None || t.perfumes = of'.perfumes;
        ]

  end

  type t = { number : int; properties : Properties.t }

  let properties (t : t) : Properties.t = t.properties

  let parse : t Angstrom.t =
    let open Angstrom in
    let* _ = string "Sue " in
    let* number = unsigned_int in
    let* _ = string ": " in
    let* properties =
      Properties.merge_all <$> sep_by1 (string ", ") Properties.parse
    in
    let* _ = char '\n' in
    return { number; properties }

  let parse_all : t list Angstrom.t = Angstrom.many1 parse
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let@ sues = Angstrom.parse_string ~consume:All Sue.parse_all input in
    match
      List.find_opt
        (Sue.Properties.is_subset_of_template << Sue.properties)
        sues
    with
    | None -> Error "Sue not found!"
    | Some sue -> Ok (string_of_int sue.number)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let@ sues = Angstrom.parse_string ~consume:All Sue.parse_all input in
    match
      List.find_opt
        (Sue.Properties.is_encabulated_subset_of_template << Sue.properties)
        sues
    with
    | None -> Error "Sue not found!"
    | Some sue -> Ok (string_of_int sue.number)
end
