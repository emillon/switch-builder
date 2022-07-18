(** Pick just one a version for each package in a set. *)

type t

val always_latest : t
val override : OpamPackage.t -> t -> t

val apply :
  t -> OpamPackage.Version.Set.t OpamPackage.Name.Map.t -> OpamPackage.Set.t
