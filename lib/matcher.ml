module Alpha =
struct
  type t =
    | Chr of char
    | Eof

  let compare c1 c2 =
    match c1, c2 with
    | Chr c1, Chr c2 -> Char.compare c1 c2
    | Chr _ , Eof    -> -1
    | Eof   , Chr _  -> 1
    | Eof   , Eof    -> 0

  let code c =
    match c with
    | Chr c -> Char.code c
    | Eof   -> 256

  let chr i =
    if i = 256
    then Eof
    else Chr (Char.chr i)
end

module NfaChar = Nfa.Make(Alpha)
module SetInt = Set.Make(Int)
module SetAlpha = Set.Make(Alpha)

