(**
   From a self-contained set, build a smaller one from packages closer to [ocaml].
*)

val cutoff : Repository.t -> int -> OpamPackage.Set.t -> OpamPackage.Set.t
