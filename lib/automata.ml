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
  module AS = Setutils.Make(A)
  module Q  = Int
  module QS = Setutils.Make(Q)

  type chr = A.t
  type str = chr list

  module DFA =
  struct
    type t =
      { (* states : QS.t ; *)
        (* alpha  : AS.t ; *)
        init   : Q.t  ;
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

  module NFA =
  struct
    type ta =
      | Sym of A.t
      | Eps

    type t =
      { init  : Q.t  ;
        final : QS.t ;
        step  : Q.t -> ta -> QS.t
      }

    let step nfa = nfa.step

    let eps_closure nfa (s : QS.t) =
      let rec go t =
        let t' = QS.map_union (fun s -> nfa.step s Eps) t in
        let dif = QS.diff t' t in
        if QS.is_empty dif
        then t
        else go t'
      in
      go s

  end
end
