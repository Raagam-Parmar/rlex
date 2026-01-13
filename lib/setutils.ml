module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type S =
sig
  include Set.S
  val map_union : (elt -> t) -> t -> t
end

module Make (Ord : OrderedType) =
struct
  module O = Set.Make(Ord)
  include O

  let map_union f set =
    let set_list_map =
      List.map
        f
        (O.to_list set)
    in
    List.fold_left
      O.union
      O.empty
      set_list_map
end
