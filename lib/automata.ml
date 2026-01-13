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
      { states : QS.t ;
        init   : Q.t  ;
        alpha  : AS.t ;
        final  : QS.t ;
        step   : Q.t -> A.t -> Q.t
      }

    type err =
      | EmptyStates
      | EmptyAlpha
      | InvalidInit of Q.t * QS.t
      | InvalidFinal of QS.t * QS.t
      | InvalidStep of Q.t * A.t * Q.t * QS.t

    let step dfa = dfa.step

    let step' dfa (q : Q.t) (str : str) =
      List.fold_left dfa.step q str

    let step_table states alpha step  =
      let neighbours q =
        AS.to_list alpha
        |> List.map (fun a -> (q, a, step q a))
      in

      QS.to_list states
      |> List.map neighbours
      |> List.concat

    (* TODO Do the same thing here *)
    let accepts dfa (str : str) =
      let final = step' dfa dfa.init str in
      QS.mem final dfa.final

    let mk_dfa states init alpha final step =
      if QS.is_empty states then
        Error EmptyStates

      else if AS.is_empty alpha then
        Error EmptyAlpha

      else if not (QS.mem init states) then
        Error (InvalidInit (init, states))

      else if not (QS.subset final states) then
        Error (InvalidFinal (final, states))

      else
        let table = step_table states alpha step in

        let invalid_trans =
          List.find_opt (fun (_, _, q) -> QS.mem q states) table
        in

        match invalid_trans with
        | Some (q1, a, q2) ->
          Error (InvalidStep (q1, a, q2, states))
        | None ->
          Ok {
            states = states;
            init   = init  ;
            alpha  = alpha ;
            final  = final ;
            step   = step
          }
  end

  module NFA =
  struct
    type ta =
      | Sym of A.t
      | Eps

    type t =
      { states : QS.t;
        init  : Q.t  ;
        alpha : AS.t ;
        final : QS.t ;
        step  : Q.t -> ta -> QS.t
      }

    type err =
      | EmptyStates
      | EmptyAlpha
      | InvalidInit of Q.t * QS.t
      | InvalidFinal of QS.t * QS.t
      | InvalidStep of Q.t * ta * QS.t * QS.t
      | AlphaMismatch of AS.t * AS.t

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

    let accepts nfa str =
      let final = step' nfa nfa.init str in
      let inter = QS.inter final nfa.final in
      not (QS.is_empty inter)

    let cardinal nfa = QS.cardinal nfa.states

    let step_table states alpha step  =
      let neighbours q =
        AS.to_list alpha
        |> List.map (fun a -> Sym a)
        |> List.cons Eps
        |> List.map (fun a -> (q, a, step q a))
      in

      QS.to_list states
      |> List.map neighbours
      |> List.concat

    let mk_nfa states init alpha final step =
      if QS.is_empty states then
        Error EmptyStates

      else if AS.is_empty alpha then
        Error EmptyAlpha

      else if not (QS.mem init states) then
        Error (InvalidInit (init, states))

      else if not (QS.subset final states) then
        Error (InvalidFinal (final, states))

      else
        let table = step_table states alpha step in

        let invalid_trans =
          List.find_opt
            (fun (_, _, qs) ->
               not (QS.is_empty (QS.diff states qs)))
            table
        in

        match invalid_trans with
        | Some (q1, a, qs) ->
          Error (InvalidStep (q1, a, qs, states))
        | None ->
          Ok {
            states = states;
            init   = init  ;
            alpha  = alpha ;
            final  = final ;
            step   = step
          }

    let iso f f_inv nfa =
      { states = nfa.states;
        init   = f nfa.init;
        alpha  = nfa.alpha;
        final  = QS.map f nfa.final;
        step   = fun q a -> QS.map f (nfa.step (f_inv q) a)
      }

    let iso_n nfa n =
      let sl = QS.to_list nfa.states in
      let f q =
        List.find_index ((=) q) sl
        |> Option.get
        |> (+) n
      in
      let f_inv q =
        List.nth sl (q - n)
      in
      iso f f_inv nfa

    let union nfa1 nfa2 =
      if nfa1.alpha <> nfa2.alpha then
        Error (AlphaMismatch (nfa1.alpha, nfa2.alpha))
      else
        let n1 = cardinal nfa1 in
        let n2 = cardinal nfa2 in
        let nfa1 = iso_n nfa1 1 in
        let nfa2 = iso_n nfa2 (n1 + 1) in
        Ok
          { states = QS.union nfa1.states nfa2.states |> QS.add 0;
            init   = 0;
            alpha  = nfa1.alpha;
            final  = QS.union nfa1.final nfa2.final;
            step   =
              fun q s ->
                if q = 0 then
                  match s with
                  | Eps   -> QS.of_list [nfa1.init; nfa2.init]
                  | Sym _ -> QS.empty

                else if 1 <= q && q <= n1 then
                  nfa1.step q s

                else if n1 + 1 <= q && q <= n1 + n2 then
                  nfa2.step q s

                else QS.empty
          }
  end
end
