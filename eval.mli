(** Evaluate pieces of formulas. *)

val rv :
  OpamParserTypes.relop * OpamPackage.Version.t -> OpamPackage.Version.t -> bool

val m :
  ('a -> (unit, 'b) Result.t) -> 'a OpamFormula.formula -> (unit, 'b) Result.t
