include Graph.Persistent.Digraph.Concrete (OpamPackage)

let load root packages =
  let add_edge_opt g = function None -> g | Some e -> add_edge_e g e in
  OpamPackage.Name.Map.fold
    (fun name version acc ->
      let pkg = OpamPackage.create name version in
      let opam = Repository.read_opam root pkg in
      let depends = OpamFile.OPAM.depends opam in
      let env =
        Env.common
        |> Env.extend "version"
             (OpamVariable.string (OpamPackage.Version.to_string version))
      in
      let filtered_depends = OpamFilter.filter_formula env depends in
      OpamFormula.fold_left
        (fun acc (name, _) ->
          let eo =
            Option.map
              (fun version ->
                let src = OpamPackage.create name version in
                (src, pkg))
              (OpamPackage.Name.Map.find_opt name packages)
          in
          add_edge_opt acc eo)
        acc filtered_depends)
    packages empty
