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

  val cmp_ts : ts -> ts -> int

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep
    | AlphaMismatch

  val mk_nfa :
    states    : qset
    -> init  : q
    -> alpha : sset
    -> final : qset
    -> step  : (q -> ts -> qset)
    -> (t, err) Result.t

  val states  : t -> qset
  val init    : t -> q
  val alpha   : t -> sset
  val final   : t -> qset
  val step    : t -> q -> s -> (qset, err) Result.t
  val step'   : t -> q -> s list -> (qset, err) Result.t
  val step_tbl : t -> (q * ts * qset) list
  val accepts : t -> s list -> (bool, err) Result.t

  val closure : t -> qset -> qset
  val cardinal : t -> int

  val union : t -> t -> (t, err) Result.t
  val concat : t -> t -> (t, err) Result.t
  val kstar : t -> t
  val complement : t -> t
end

module Make
    (A : ALPHABET_TYPE) =
struct
  module AS = Setutils.Make(A)

  type s    = A.t
  type sset = AS.t

  type ts =
    | Sym of s
    | Eps

  let cmp_ts s1 s2 =
    match s1, s2 with
    | Sym s1, Sym s2 -> A.compare s1 s2
    | Sym _ , Eps    -> 1
    | Eps   , Sym _  -> -1
    | Eps   , Eps    -> 0

  module Q  = Int
  module QS = Setutils.Make(Q)

  type q    = Q.t
  type qset = QS.t

  type t =
    { states : qset ;
      init   : q    ;
      alpha  : sset ;
      final  : qset ;
      step   : q -> ts -> qset
    }

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep
    | AlphaMismatch

  let states nfa = nfa.states
  let init nfa = nfa.init
  let alpha nfa = nfa.alpha
  let final nfa = nfa.final
  let cardinal nfa = QS.cardinal nfa.states

  let step nfa q s =
    if not (AS.mem s nfa.alpha)
    then Error InvalidChar
    else Ok (nfa.step q (Sym s))

  let closure nfa qs =
    let rec go t =
      let t' =
        QS.map_union (fun s -> nfa.step s Eps) t in
      let dif = QS.diff t' t in
      if QS.is_empty dif
      then t
      else go t'
    in
    go qs

  let ( let* ) = Result.bind

  let step' nfa q str =
    let valid =
      List.for_all
        (fun s -> AS.mem s nfa.alpha)
        str
    in
    if not valid
    then Error InvalidChar
    else
      let rec go q str =
        match str with
        | []        -> closure nfa (QS.singleton q)
        | s :: str' ->
          let p = nfa.step q (Sym s) in
          let reachable = closure nfa p in
          QS.map_union (fun r -> go r str') reachable
      in
      Ok (go q str)

  let step_tbl nfa  =
    let neighbours q =
      AS.to_list nfa.alpha
      |> List.map (fun a -> Sym a)
      |> List.cons Eps
      |> List.map (fun a -> (q, a, nfa.step q a))
    in

    QS.to_list nfa.states
    |> List.map neighbours
    |> List.concat

  let accepts nfa str =
    let valid = List.for_all (fun s -> AS.mem s nfa.alpha) str in
    if not valid then
      Error InvalidChar
    else
      let* final = step' nfa nfa.init str in
      let inter = QS.inter final nfa.final in
      Ok (not (QS.is_empty inter))

  let chk_states nfa =
    if QS.is_empty nfa.states
    then Error EmptyStates
    else Ok nfa

  let chk_init nfa =
    if not (QS.mem nfa.init nfa.states)
    then Error InvalidInit
    else Ok nfa

  let chk_alpha nfa =
    if AS.is_empty nfa.alpha
    then Error EmptyAlpha
    else Ok nfa

  let chk_final nfa =
    if not (QS.subset nfa.final nfa.states)
    then  Error InvalidFinal
    else Ok nfa

  let chk_step nfa =
    let symbol_l =
      AS.to_list nfa.alpha
      |> List.map (fun s -> Sym s)
    in
    let invalid =
      nfa.states
      |> QS.exists (fun q ->
          List.exists (fun a ->
              let ps = nfa.step q a in
              QS.is_empty (QS.diff nfa.states ps)
            ) symbol_l
        )
    in
    if invalid
    then Error InvalidStep
    else Ok nfa

  let chk_nfa nfa =
    let* nfa = chk_states nfa in
    let* nfa = chk_init   nfa in
    let* nfa = chk_alpha  nfa in
    let* nfa = chk_final  nfa in
    chk_step nfa

  let mk_nfa ~states ~init ~alpha ~final ~step =
    chk_nfa
      { states = states;
        init   = init;
        alpha  = alpha;
        final  = final;
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
