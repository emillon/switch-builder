(** Utility functions available in this project. *)

type 'a fmt := Format.formatter -> 'a -> unit

val pp_package : OpamPackage.t fmt
val pp_name : OpamPackage.Name.t fmt
val pp_version : OpamPackage.Version.t fmt
val pp_package_set : OpamPackage.Set.t fmt
val fixpoint : equal:('a -> 'a -> bool) -> f:('a -> 'a) -> 'a -> 'a

module Result_O : sig
  val ( let* ) :
    ('a, 'e) Result.t -> ('a -> ('b, 'e) Result.t) -> ('b, 'e) Result.t
end

val to_version_map :
  OpamPackage.Set.t -> OpamPackage.Version.t OpamPackage.Name.Map.t
