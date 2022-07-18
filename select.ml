type filter = OpamPackage.t -> OpamFile.OPAM.t -> bool

let apply repo filters =
  OpamPackage.Set.filter (fun pkg ->
      let opam = Repository.read_opam repo pkg in
      List.fold_left (fun acc filter -> acc && filter pkg opam) true filters)

let has_no_depexts _pkg opam =
  let depexts = OpamFile.OPAM.depexts opam in
  depexts = []

let is_compatible_with target_pkg _pkg opam =
  let depends =
    OpamFile.OPAM.depends opam |> OpamFilter.filter_formula Env.common
  in
  let target_name = OpamPackage.name target_pkg in
  let target_version = OpamPackage.version target_pkg in
  let evaluated_depends =
    OpamFormula.partial_eval
      (fun (name, vf) ->
        if OpamPackage.Name.equal name target_name then
          OpamFormula.partial_eval
            (fun rv -> if Eval.rv rv target_version then `True else `False)
            vf
        else `Formula (Atom (name, vf)))
      depends
  in
  match evaluated_depends with
  | `True -> true
  | `False -> false
  | `Formula _ -> true

let exclude_packages pkgs pkg _ =
  not (List.exists (fun p -> OpamPackage.equal p pkg) pkgs)

let exclude_package_names names pkg _ =
  let pkg_name = OpamPackage.name pkg in
  not (List.exists (fun name -> OpamPackage.Name.equal name pkg_name) names)

let exclude_package_prefix prefix pkg _ =
  not
    (String.starts_with ~prefix
       (OpamPackage.Name.to_string (OpamPackage.name pkg)))
