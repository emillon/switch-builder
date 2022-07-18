(**
   Filter packages using per-package properties.

   These apply to packages in isolation, such as "is compatible with X" or "has
   this name". For properties that refer to the set as a whole, see the {!Check}
   module.
*)

type filter

val apply :
  Repository.t -> filter list -> OpamPackage.Set.t -> OpamPackage.Set.t

val has_no_depexts : filter
val is_compatible_with : OpamPackage.t -> filter
val exclude_packages : OpamPackage.t list -> filter
val exclude_package_names : OpamPackage.Name.t list -> filter
val exclude_package_prefix : string -> filter
