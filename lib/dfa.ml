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

  type t =
    { states : QS.t ;
      init   : Q.t  ;
      alpha  : AS.t ;
      final  : QS.t ;
      step   : Q.t -> A.t -> Q.t
    }

  type err =
    | InvalidString of str * AS.t
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

  let accepts dfa (str:str) =
    let valid = List.for_all (fun s -> AS.mem s dfa.alpha) str in
    if not valid then
      Error (InvalidString (str, dfa.alpha))
    else
      let final = step' dfa dfa.init str in
      Ok (QS.mem final dfa.final)


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
        List.find_opt (fun (_, _, q) -> not (QS.mem q states)) table
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
