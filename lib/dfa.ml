module type ALPHABET_TYPE =
sig
  type t
  val compare : t -> t -> int
end

module type STATE_TYPE =
sig
  type t
  val compare : t -> t -> int
end

module type DFA_TYPE =
sig
  type t

  type s
  type q

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep

  val mk_dfa :
    states   : q list
    -> init  : q
    -> alpha : s list
    -> final : q list
    -> step  : (q -> s -> q)
    -> (t, err) Result.t

  val states  : t -> q list
  val init    : t -> q
  val alpha   : t -> s list
  val final   : t -> q list
  val step    : t -> q -> s -> (q, err) Result.t
  val step'   : t -> q -> s list -> (q, err) Result.t
  val accepts : t -> s list -> (bool, err) Result.t
end

module Make
    (A : ALPHABET_TYPE)
    (Q : STATE_TYPE)
  : DFA_TYPE =
struct
  module AS = Setutils.Make(A)
  module QS = Setutils.Make(Q)

  type s = A.t
  type q = Q.t
  type str = s list

  type t =
    { states : QS.t ;
      init   : Q.t  ;
      alpha  : AS.t ;
      final  : QS.t ;
      step   : Q.t -> A.t -> Q.t
    }

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep


  let states dfa = QS.to_list dfa.states
  let init dfa = dfa.init
  let alpha dfa = AS.to_list dfa.alpha
  let final dfa = QS.to_list dfa.final

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
      { states = QS.of_list states;
        init   = init;
        alpha  = AS.of_list alpha;
        final  = QS.of_list final;
        step   = step
      }
end
