(** Check that a package set is coinstallable.

    This does not rely on opam's solver.
*)

type error
(** A reason why a particular package is not installable. *)

val pp_error : Format.formatter -> error -> unit

val apply :
  Repository.t -> OpamPackage.Set.t -> (unit, error) result OpamPackage.Map.t
(** Check whether this package set is coinstallable. This checks that:
    - the [available] field evaluates to [true]
    - the [depends] field is consistent
    - no clause in [conflicts] is satisfied
    - [conflicts-class] are consistent
    *)

val remove_packages_with_errors :
  (unit, error) result OpamTypes.package_map -> OpamPackage.Set.t
(** In the result of [apply], remove packages with issues. The resulting set
    might not be coinstallable: [Closure.apply] can fix this. *)
