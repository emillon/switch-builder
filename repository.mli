(** Sets of opam packages. This is a stateless representation that does not use
    opam's notion of repositories. *)

type t

val path : string -> t
(** A checkout of an opam repository, with the usual layout, ie containing
    [packages/$NAME/$NAME.$VERSION/opam] files. *)

val combine : t -> t -> t
(** Combine two repositories. This has shadowing semantics: if a package is
    available in [a] and [b], the definition for this package in [combine a b]
    is picked from [a]. *)

val packages : t -> OpamPackage.Version.Set.t OpamPackage.Name.Map.t
val read_opam : t -> OpamPackage.t -> OpamFile.OPAM.t

module Stats : sig
  type t

  val create : unit -> t
  val pp : Format.formatter -> t -> unit
end

val stats : Stats.t -> t -> t
(** Instrument a repository to measure operation counts. *)

module Cache : sig
  type t

  val create : unit -> t
end

val cache : Cache.t -> t -> t
(** Cache the [read_opam] operation. *)
