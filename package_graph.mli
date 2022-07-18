(** Interface to ocamlgraph.

    A package graph here is a graph where:
    - vertices are packages (with a version, ie a [OpamPackage.t])
    - edges [a -> b] mean that [b] appears in [a]'s dependencies
*)

include Graph.Sig.P with type V.t = OpamPackage.t

val load : Repository.t -> OpamPackage.Version.t OpamPackage.Name.Map.t -> t
