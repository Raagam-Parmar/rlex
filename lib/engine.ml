open Pattern
open Matcher

module Position =
struct
  type t =
    { pos_fname : string
    ; pos_cnum : int
    ; pos_lnum : int
    ; pos_bol : int
    }

  let zero =
    { pos_fname=""
    ; pos_cnum=0
    ; pos_lnum=0
    ; pos_bol=0
    }
end


module Lexbuf =
struct
  type t =
    { lex_buf : string
    ; lex_buf_len : int
    ; mutable lex_start_p : Position.t
    (** Points to the next character in the buffer to be scanned. *)
    ; mutable lex_curr_p : Position.t
    ; mutable eof_reached : bool
    }

  let lexbuf_of_string str =
    { lex_buf=str
    ; lex_buf_len=String.length str
    ; lex_start_p=Position.zero
    ; lex_curr_p=Position.zero
    ; eof_reached=false
    }

  let next_char lexbuf =
    let i = lexbuf.lex_curr_p.pos_cnum in
    if i >= lexbuf.lex_buf_len
    then None
    else Some lexbuf.lex_buf.[i]

  let unsafe_next_char lexbuf =
    let i = lexbuf.lex_curr_p.pos_cnum in
    lexbuf.lex_buf.[i]

  let advance lexbuf =
    let i = lexbuf.lex_curr_p.pos_cnum in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = i + 1 }
end


module Rules =
struct
  type 'a action = Lexbuf.t -> 'a

  type 'a branch =
    { branch_pat : pattern
    ; branch_act : 'a action
    ; branch_nfa : NfaChar.t
    ; branch_stt : SetInt.t
    }

  type 'a rule = 'a branch list

  let mk_branch (pat, act) =
    let n = matcher pat in
    { branch_pat=pat
    ; branch_act=act
    ; branch_nfa=n
    ; branch_stt=NfaChar.init_cls n
    }

  let mk_rule pat_act : 'a rule =
    List.map mk_branch pat_act
end
