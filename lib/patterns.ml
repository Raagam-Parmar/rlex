type pattern =
  | PNull
  | PEps
  | PEof
  | PChr    of char
  | PUnion  of pattern * pattern
  | PConcat of pattern * pattern
  | PStar   of pattern
  | PCompl  of pattern
