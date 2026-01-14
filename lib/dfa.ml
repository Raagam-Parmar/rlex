module type ALPHABET_TYPE =
sig
  type t
  val compare : t -> t -> int
end

module type DFA_TYPE =
sig
  type t

  type q
  type qset

  type s
  type sset

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep

  val mk_dfa :
    states   : qset
    -> init  : q
    -> alpha : sset
    -> final : qset
    -> step  : (q -> s -> q)
    -> (t, err) Result.t

  val states  : t -> qset
  val init    : t -> q
  val alpha   : t -> sset
  val final   : t -> qset
  val step    : t -> q -> s -> (q, err) Result.t
  val step'   : t -> q -> s list -> (q, err) Result.t
  val step_tbl : t -> (q * s * q) list

  val accepts : t -> s list -> (bool, err) Result.t

  val cardinal : t -> int
end

module Make
    (A : ALPHABET_TYPE)
  : DFA_TYPE =
struct
  module AS = Setutils.Make(A)

  type s    = A.t
  type sset = AS.t

  module Q  = Int
  module QS = Setutils.Make(Q)

  type q    = Q.t
  type qset = QS.t

  type t =
    { states : qset ;
      init   : q    ;
      alpha  : sset ;
      final  : qset ;
      step   : q -> s -> q
    }

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep


  let states dfa = dfa.states
  let init dfa = dfa.init
  let alpha dfa = dfa.alpha
  let final dfa = dfa.final
  let cardinal dfa = QS.cardinal dfa.states

  let step dfa q s =
    if not (AS.mem s dfa.alpha) then
      Error InvalidChar
    else
      Ok (dfa.step q s)

  let ( let* ) = Result.bind

  let rec step' dfa q str =
    match str with
    | []        -> Ok q
    | s :: str' ->
      let* p = step dfa q s in
      step' dfa p str'

  let step_tbl dfa =
    let neighbours q =
      AS.to_list dfa.alpha
      |> List.map (fun a -> (q, a, dfa.step q a))
    in

    QS.to_list dfa.states
    |> List.map neighbours
    |> List.concat

  let accepts dfa str =
    let valid = List.for_all (fun s -> AS.mem s dfa.alpha) str in
    if not valid then
      Error InvalidChar
    else
      let* final = step' dfa dfa.init str in
      Ok (QS.mem final dfa.final)

  let chk_states dfa =
    if QS.is_empty dfa.states then
      Error EmptyStates
    else
      Ok dfa

  let chk_init dfa =
    if not (QS.mem dfa.init dfa.states)
    then Error InvalidInit
    else Ok dfa

  let chk_alpha dfa =
    if AS.is_empty dfa.alpha then
      Error EmptyAlpha
    else
      Ok dfa

  let chk_final dfa =
    if not (QS.subset dfa.final dfa.states) then
      Error InvalidFinal
    else
      Ok dfa

  let chk_step dfa =
    let bad =
      QS.exists
        (fun q ->
           AS.exists
             (fun a -> not (QS.mem (dfa.step q a) dfa.states))
             dfa.alpha)
        dfa.states
    in
    if bad then
      Error InvalidStep
    else Ok dfa

  let chk_dfa dfa =
    let* dfa = chk_states dfa in
    let* dfa = chk_init   dfa in
    let* dfa = chk_alpha  dfa in
    let* dfa = chk_final  dfa in
    chk_step dfa

  let mk_dfa ~states ~init ~alpha ~final ~step =
    chk_dfa
      { states = states;
        init   = init;
        alpha  = alpha;
        final  = final;
        step   = step
      }
end
