open Rlex.Pattern
open Rlex.Pattern.Infix
open Rlex.Matcher

let _print_set s = SetInt.fold (fun q str -> str ^ Int.to_string q) s ""

let _print_tbl =
  List.fold_left
    (fun acc (q, ts, qset) ->
       Printf.sprintf
         "%s(%d, %s, %s)\n"
         acc
         q
         (match ts with
          | NfaChar.Sym c ->
            (match c with
             | Alpha.Chr c' -> Printf.sprintf "%s" (Char.escaped c')
             | Alpha.Eof -> "[EOF]")
          | NfaChar.Eps -> "[EPS]")
         (_print_set qset))
    ""

let pat_digit = chr_set [('0', '9'); ('a', 'b')]
let pat_int = !+ pat_digit
let nfa = matcher pat_int

let _test =
  let t =
    [ "2ababab"
    ; "0808"
    ; "95"
    ]
  in
  let a = accepts nfa in
  let b = List.map a t in
  let c = List.map Result.get_ok b in
  List.iter (fun b -> print_endline (Bool.to_string b)) c
