open Pattern

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

  let of_string str =
    String.fold_left (fun sl c -> sl @ [Chr c]) [] str
end

module NfaChar = Nfa.Make(Alpha)
module SetInt = Set.Make(Int)
module SetAlpha = Set.Make(Alpha)

exception UnexpectedError of string

let unpack e str =
  match e with
  | Error e -> failwith (str ^ "\n" ^ (NfaChar.fmt_err e))
  | Ok r -> r

let all_symbols =
  List.init 257 (fun i -> i)
  |> List.map Alpha.chr
  |> SetAlpha.of_list

let accept_null =
  let n =
    NfaChar.mk_nfa
      ~states:(SetInt.singleton 0)
      ~init:0
      ~alpha:all_symbols
      ~final:SetInt.empty
      ~step:(fun _ _ -> SetInt.empty)
  in
  unpack n "Matcher.accept_null : unexpected failure"

let accept_eps =
  let n =
    NfaChar.mk_nfa
      ~states:(SetInt.singleton 0)
      ~init:0
      ~alpha:all_symbols
      ~final:(SetInt.singleton 0)
      ~step:(fun _ _ -> SetInt.empty)
  in
  unpack n "Matcher.accept_eps : unexpected failure"

let accept_chr c =
  let n =
    NfaChar.mk_nfa
      ~states:(SetInt.of_list [0; 1])
      ~init:0
      ~alpha:all_symbols
      ~final:(SetInt.singleton 1)
      ~step:
        (fun q s ->
           if q = 0 then
             if NfaChar.cmp_ts s (NfaChar.Sym c) = 0
             then SetInt.singleton 1
             else SetInt.empty
           else
             SetInt.empty
        )
  in
  unpack n "Matcher.accept_chr : unexpected failure"

let rec matcher p =
  match p with
  | PNull -> accept_null
  | PEps -> accept_eps
  | PEof -> accept_chr Alpha.Eof
  | PChr c -> accept_chr (Alpha.Chr c)
  | PUnion (p1, p2) ->
    let n1 = matcher p1 in
    let n2 = matcher p2 in
    let n = NfaChar.union n1 n2 in
    unpack n "Matcher.matcher : unexpected failure, union"

  | PConcat (p1, p2) ->
    let n1 = matcher p1 in
    let n2 = matcher p2 in
    let n = NfaChar.concat n1 n2 in
    unpack n "Matcher.matcher : unexpected failure, concat"

  | PStar p ->
    let n = matcher p in
    NfaChar.kstar n

let accepts n str =
  Alpha.of_string str
  |> NfaChar.accepts n
