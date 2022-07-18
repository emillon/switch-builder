let pp_name ppf name = Format.fprintf ppf "%s" (OpamPackage.Name.to_string name)

let pp_version ppf version =
  Format.fprintf ppf "%s" (OpamPackage.Version.to_string version)

let pp_package ppf pkg = Format.fprintf ppf "%s" (OpamPackage.to_string pkg)

let pp_package_set ppf set =
  OpamPackage.Set.iter (fun pkg -> Format.fprintf ppf "%a\n" pp_package pkg) set

let rec fixpoint ~equal ~f start =
  let next = f start in
  if equal start next then next else fixpoint ~equal ~f next

module Result_O = struct
  let ( let* ) x f = Result.bind x f
end

let to_version_map set =
  OpamPackage.Set.fold
    (fun pkg acc ->
      let name = OpamPackage.name pkg in
      let version = OpamPackage.version pkg in
      OpamPackage.Name.Map.add name version acc)
    set OpamPackage.Name.Map.empty
