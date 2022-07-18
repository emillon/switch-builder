(** The environment used to evaluate formulas (contains variables like [arch],
    [os], etc). *)

val common : OpamFilter.env

val extend :
  string -> OpamVariable.variable_contents -> OpamFilter.env -> OpamFilter.env
