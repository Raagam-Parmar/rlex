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

    let eps_closure nfa qs =
      let rec go t =
        let t' =
          QS.map_union (fun s -> nfa.step s Eps) t in
        let dif = QS.diff t' t in
        if QS.is_empty dif
        then t
        else go t'
      in
      go qs

    let rec step' nfa q str =
      match str with
      | []      -> eps_closure nfa (QS.singleton nfa.init)
      | s::str' ->
        let reachable = eps_closure nfa (nfa.step q (Sym s)) in
        QS.map_union (fun r -> step' nfa r str') reachable

    let isomorph f f_inv nfa =
      { init  = f nfa.init;
        final = QS.map f nfa.final;
        step  = fun q a -> QS.map f (nfa.step (f_inv q) a)
      }

    let accepts nfa str =
      let final = step' nfa nfa.init str in
      let inter = QS.inter final nfa.final in
      not (QS.is_empty inter)
  end
end
