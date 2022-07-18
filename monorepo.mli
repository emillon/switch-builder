(** Transform a package set into an [opam-monorepo] compatible lockfile.

    A monorepo is made of two parts:
    - a set of packages installed in an opam switch in a classic way
    - a set of packages that are unpacked in a [duniverse] directory in the dune
    workspace

    This comes with important properties:
    - packages in the workspace need to build correctly with dune
    - no package in the opam switch can depend on a package in the workspace

    The {!analyze} function builds a monorepo that respects these properties.
   *)

type t

val analyze : Repository.t -> OpamPackage.Set.t -> t
val pp : Format.formatter -> t -> unit
val opam_file : t -> OpamFile.OPAM.t
