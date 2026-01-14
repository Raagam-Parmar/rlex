module type ALPHABET_TYPE =
sig
  type t
  val compare: t -> t -> int
end

module type NFA_TYPE =
sig
  type t

  type q
  type qset

  type s
  type sset

  type ts =
    | Sym of s
    | Eps

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep

  val mk_nfa :
    stats    : qset
    -> init  : q
    -> alpha : sset
    -> final : qset
    -> step  : (q -> ts -> qset)
    -> (t, err) Result.t

  val states  : t -> qset
  val init    : t -> q
  val alpha   : t -> sset
  val step    : t -> q -> s -> (qset, err) Result.t
  val step'   : t -> q -> s list -> (qset, err) Result.t
  val accepts : t -> s list -> (bool, err) Result.t
end

module Make (A : ALPHABET_TYPE) =
struct
  module AS = Setutils.Make(A)

  type s    = A.t
  type sset = AS.t
  type str  = s list

  module Q  = Int
  module QS = Setutils.Make(Q)

  type q    = Q.t
  type qset = QS.t

  type ta =
    | Sym of s
    | Eps

  type t =
    { states : qset ;
      init   : q    ;
      alpha  : sset ;
      final  : qset ;
      step   : q -> ta -> qset
    }

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep
    | AlphaMismatch

  let step nfa q s =
    if not (AS.mem s nfa.alpha)
    then Error InvalidChar
    else Ok (nfa.step q (Sym s))

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
    let valid = List.for_all (fun s -> AS.mem s nfa.alpha) str in
    if not valid then
      Error InvalidChar
    else
      let final = step' nfa nfa.init str in
      let inter = QS.inter final nfa.final in
      Ok (not (QS.is_empty inter))

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
      Error InvalidInit

    else if not (QS.subset final states) then
      Error InvalidFinal

    else
      let table = step_table states alpha step in

      let invalid_trans =
        List.find_opt
          (fun (_, _, qs) ->
             not (QS.is_empty (QS.diff states qs)))
          table
      in

      match invalid_trans with
      | Some (_, _, _) ->
        Error InvalidStep
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
    if AS.compare nfa1.alpha nfa2.alpha <> 0 then
      Error AlphaMismatch
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

  let concat nfa1 nfa2 =
    if AS.compare nfa1.alpha nfa2.alpha <> 0 then
      Error AlphaMismatch
    else
      let n1 = cardinal nfa1 in
      let n2 = cardinal nfa2 in
      let nfa1 = iso_n nfa1 0 in
      let nfa2 = iso_n nfa2 n1 in
      Ok
        { states = QS.union nfa1.states nfa2.states;
          init   = nfa1.init;
          alpha  = nfa1.alpha;
          final  = nfa2.final;
          step   =
            fun q s ->
              if QS.mem q nfa1.final then
                match s with
                | Eps -> nfa1.step q s |> QS.add nfa1.init
                | Sym _ -> nfa1.step q s

              else if 0 <= q && q <= n1 - 1 then
                nfa1.step q s

              else if n1 <= q && q <= n2 - 1 then
                nfa2.step q s

              else QS.empty
        }

  let kstar nfa =
    let n = cardinal nfa in
    let nfa = iso_n nfa 1 in
    { states = QS.add 0 nfa.states;
      init   = 0;
      alpha  = nfa.alpha;
      final  = QS.singleton 0;
      step   =
        fun q s ->
          if q = 0 then
            match s with
            | Eps   -> QS.singleton nfa.init
            | Sym _ -> QS.empty

          else if QS.mem q nfa.final then
            match s with
            | Eps   -> nfa.step q s |> QS.add 0
            | Sym _ -> nfa.step q s

          else if 0 <= q && q <= n - 1 then
            nfa.step q s

          else QS.empty
    }

  let complement nfa =
    { states = nfa.states;
      init   = nfa.init;
      alpha  = nfa.alpha;
      final  = QS.diff nfa.states nfa.final;
      step   = nfa.step
    }
end
