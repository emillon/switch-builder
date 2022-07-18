open Import

module Weight = struct
  include Int

  type edge = Package_graph.E.t

  let weight _ = -1
end

module BF = Graph.Path.BellmanFord (Package_graph) (Weight)

let find_ocaml packages =
  let name = OpamPackage.Name.of_string "ocaml" in
  let version = OpamPackage.Name.Map.find name packages in
  OpamPackage.create name version

let cutoff repo n packages =
  let package_versions = to_version_map packages in
  let graph = Package_graph.load repo package_versions in
  let root_pkg = find_ocaml package_versions in
  let distances = BF.all_shortest_paths graph root_pkg in
  OpamPackage.Set.filter
    (fun pkg ->
      match BF.H.find_opt distances pkg with
      | Some dist -> -dist <= n
      | None -> true)
    packages
