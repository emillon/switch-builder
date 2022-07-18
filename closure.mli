(** Reduce to a self-contained set.

    That is, remove all dependencies that are not contained in the set itself.
*)

val apply : Repository.t -> OpamPackage.Set.t -> OpamPackage.Set.t
