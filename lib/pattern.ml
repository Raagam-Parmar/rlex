type pattern =
  | PNull
  | PEps
  | PEof
  | PChr    of char
  | PUnion  of pattern * pattern
  | PConcat of pattern * pattern
  | PStar   of pattern

let null = PNull
let eps = PEps
let eof = PEof
let chr c = PChr c
let union p1 p2 = PUnion (p1, p2)
let concat p1 p2 = PConcat (p1, p2)
let star p = PStar p

let chr_range c1 c2 =
  let cc1 = Char.code c1 in
  let cc2 = Char.code c2 in
  let smaller = min cc1 cc2 in
  let rng = Int.abs (cc1 - cc2) in
  List.init (rng + 1) (fun n -> Char.chr (smaller + n))
  |> List.map chr
  |> List.fold_left union null

let chr_set ccl =
  ccl
  |> List.map (fun (c1, c2) -> chr_range c1 c2)
  |> List.fold_left union null

let str =
  String.fold_left (fun p c -> concat p (chr c)) eps

let plus p = concat p (star p)

let opt p = union p eps

module Infix =
struct
  let ( <|> ) = union
  let ( <^> ) = concat

  let ( !* ) = star
  let ( !+ ) = plus
  let ( !? ) = opt
end
