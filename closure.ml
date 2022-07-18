open Import

let contract repo set =
  let name_set =
    OpamPackage.Set.fold
      (fun pkg acc ->
        let name = OpamPackage.name pkg in
        OpamPackage.Name.Set.add name acc)
      set OpamPackage.Name.Set.empty
  in
  OpamPackage.Set.filter
    (fun pkg ->
      let opam = Repository.read_opam repo pkg in
      let depends =
        OpamFile.OPAM.depends opam |> OpamFilter.filter_formula Env.common
      in
      let r =
        Eval.m
          (fun ((name : OpamPackage.Name.t), _) ->
            if OpamPackage.Name.Set.mem name name_set then Ok () else Error name)
          depends
      in
      match r with Ok () -> true | Error _d -> false)
    set

let apply repo = fixpoint ~equal:OpamPackage.Set.equal ~f:(contract repo)
