open Pattern
open Pattern.Infix
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
