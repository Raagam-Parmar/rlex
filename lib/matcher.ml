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

exception UnepectedError of string

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
  match n with
  | Error _ ->
    raise (UnepectedError "Compile.accept_null : unexpected faliure")
  | Ok n -> n

let accept_eps =
  let n =
    NfaChar.mk_nfa
      ~states:(SetInt.singleton 0)
      ~init:0
      ~alpha:all_symbols
      ~final:(SetInt.singleton 0)
      ~step:(fun _ _ -> SetInt.empty)
  in
  match n with
  | Error _ ->
    raise (UnepectedError "Compile.accept_eps : unexpected faliure")
  | Ok n -> n

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
             if NfaChar.cmp_ts s c = 0
             then SetInt.singleton 1
             else SetInt.empty
           else
             SetInt.empty
        )
  in
  match n with
  | Error _ ->
    raise (UnepectedError "Compile.accept_eps : unexpected faliure")
  | Ok n -> n
