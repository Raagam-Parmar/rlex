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

  let next_states_branch a branch =
    let n = branch.branch_nfa in
    let state' = NfaChar.unsafe_stepset n branch.branch_stt a in
    { branch with branch_stt=state' }

  let next_states (rule : 'a rule) a =
    List.map (next_states_branch a) rule

  let is_dead_branch branch =
    SetInt.is_empty branch.branch_stt

  let is_dead (rule : 'a rule) =
    List.for_all is_dead_branch rule

  let first_match_idx rule =
    List.find_index
      (fun branch ->
         let final = NfaChar.final branch.branch_nfa in
         SetInt.inter branch.branch_stt final |> SetInt.is_empty |> not)
      rule

  let rec match_longest ?(lex_last_p_i=None) branch (lexbuf : Lexbuf.t) =
    if lexbuf.eof_reached
    then
      (* Input exhausted, EOF reached. *)
      (* Check if the currently given tracker has a match and return it.
         If not, return the last match. If no last match exists, fail. *)
      let match_idx = first_match_idx branch in
      match match_idx with
      | Some i -> i
      | None ->
        match lex_last_p_i with
        | Some (lex_last_p, i) -> (lexbuf.lex_curr_p <- lex_last_p; i)
        | None -> failwith "lexing failure.1\n"
    else
      (* Input has not been exhausted, get the next character and advance the
         buffer. *)
      let c = Lexbuf.unsafe_next_char lexbuf in
      let a = Alpha.Chr c in
      let lex_prev_p = lexbuf.lex_curr_p in
      let () = Lexbuf.advance lexbuf in
      let eof_reached = lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_buf_len in
      let () = lexbuf.eof_reached <- eof_reached in

      (* Check if any of the patterns have matched so far before scanning the
         next character, pointed to by lexbuf.lex_curr_p. *)
      let match_idx = first_match_idx branch in
      match match_idx with
      | None ->
        (* None of the current states are final. *)
        (* Check if any branch is alive, if not, backtrack to the last matched
           position. If last matched position is None, fail. *)
        if is_dead branch
        then
          match lex_last_p_i with
          | None -> failwith "lexing failure.2\n"
          | Some (lex_last_p, i) -> (lexbuf.lex_curr_p <- lex_last_p; i)
        else
          let tracker' = next_states branch a in
          match_longest ~lex_last_p_i:None tracker' lexbuf

      | Some i ->
        (* i is the index of the first action matched in the rule. *)
        let tracker' = next_states branch a in
        match_longest ~lex_last_p_i:(Some (lex_prev_p, i)) tracker' lexbuf
end
