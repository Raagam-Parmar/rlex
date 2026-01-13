module type SymbolType =
sig
  type t
  val compare: t -> t -> int
end

module type DFAType =
sig
  type t
end

module Make (A : SymbolType) =
struct
  module AS = Set.Make(A)
  module Q  = Int
  module QS = Set.Make(Q)

  type chr = A.t
  type str = chr list

  module DFA =
  struct
    type t =
      { states : QS.t ;
        init   : Q.t  ;
        alpha  : AS.t ;
        final  : QS.t ;
        step   : Q.t -> A.t -> Q.t
      }

    let step dfa = dfa.step

    let step' dfa (q : Q.t) (str : str) =
      List.fold_left dfa.step q str

    let accepts dfa (str : str) =
      let final = step' dfa dfa.init str in
      QS.mem final dfa.final
  end
end
