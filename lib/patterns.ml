type pattern =
  | PNull
  | PEps
  | PEof
  | PChr    of char
  | PUnion  of pattern * pattern
  | PConcat of pattern * pattern
  | PStar   of pattern
  | PCompl  of pattern

let null = PNull
let eps = PEps
let eof = PEof
let chr c = PChr c
let union p1 p2 = PUnion (p1, p2)
let concat p1 p2 = PConcat (p1, p2)
let star p = PStar p
let compl p = PCompl p
let inter p1 p2 = union (compl p1) (compl p2)
let diff p1 p2 = inter p1 (compl p2)

let chr_range c1 c2 =
  let len = Int.abs (Char.code c1 - Char.code c2) in
  List.init len Char.chr
  |> List.map chr
  |> List.fold_left union null

let chr_set ccl =
  ccl
  |> List.map (fun (c1, c2) -> chr_range c1 c2)
  |> List.fold_left union null

let str =
  String.fold_left (fun p c -> union p (chr c)) null

let plus p = concat p (star p)

let opt p = union p eps
