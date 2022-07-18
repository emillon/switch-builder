type strategy = Latest | Fixed of OpamPackage.Version.t
type t = OpamPackage.Name.t -> strategy

let always_latest _ = Latest

let override pkg t name =
  if OpamPackage.Name.equal name (OpamPackage.name pkg) then
    Fixed (OpamPackage.version pkg)
  else t name

let apply_to versions = function
  | Latest -> OpamPackage.Version.Set.max_elt versions
  | Fixed target_version ->
      assert (OpamPackage.Version.Set.mem target_version versions);
      target_version

let apply policy name_map =
  OpamPackage.Name.Map.fold
    (fun name versions acc ->
      let version = apply_to versions (policy name) in
      let pkg = OpamPackage.create name version in
      OpamPackage.Set.add pkg acc)
    name_map OpamPackage.Set.empty
