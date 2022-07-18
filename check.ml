open Import

let relop_to_string = function
  | `Leq -> "<="
  | `Geq -> ">="
  | `Lt -> "<"
  | `Gt -> ">"
  | `Eq -> "="
  | `Neq -> "<>"

let pp_rv ppf (relop, version) =
  Format.fprintf ppf "%s %a" (relop_to_string relop) pp_version version

let rv_to_string rv = Format.asprintf "%a" pp_rv rv

type error =
  | Available
  | Conflicts of OpamPackage.Name.t
  | Conflicts_version of
      OpamPackage.Name.t
      * (OpamTypes.relop * OpamPackage.Version.t)
      * OpamPackage.Version.t
  | Conflict_class of OpamPackage.Name.t * OpamPackage.t list
  | Dependency_missing of OpamPackage.Name.t
  | Dependency_constraint of
      OpamPackage.Name.t * OpamFormula.version_formula * OpamPackage.Version.t

let pp_error ppf = function
  | Available -> Format.fprintf ppf "available not satisfied"
  | Conflicts name -> Format.fprintf ppf "conflicts with %a" pp_name name
  | Conflicts_version (name, rv, existing_version) ->
      Format.fprintf ppf "conflicts with %a %a (got %a)" pp_name name pp_rv rv
        pp_version existing_version
  | Conflict_class (cc, pkgs) ->
      Format.fprintf ppf "conflict class %a is shared between %a" pp_name cc
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           pp_package)
        pkgs
  | Dependency_missing name ->
      Format.fprintf ppf "dependency %a not found" pp_name name
  | Dependency_constraint (name, version_formula, existing_version) ->
      Format.fprintf ppf
        "dependency %a not satisfying constraint (want %s, got %a)" pp_name name
        (OpamFormula.string_of_formula rv_to_string version_formula)
        pp_version existing_version

let handle_depends packages =
  Eval.m (fun (name, version_formula) ->
      match OpamPackage.Name.Map.find_opt name packages with
      | None -> Error (Dependency_missing name)
      | Some existing_version ->
          let ok =
            OpamFormula.eval
              (fun (relop, version) ->
                OpamFormula.eval_relop relop existing_version version)
              version_formula
          in
          if ok then Ok ()
          else
            Error
              (Dependency_constraint (name, version_formula, existing_version)))

let handle_conflict packages (name, cstro) =
  match (OpamPackage.Name.Map.find_opt name packages, cstro) with
  | None, _ -> Ok ()
  | Some _, None -> Error (Conflicts name)
  | Some existing_version, Some rv ->
      if Eval.rv rv existing_version then
        Error (Conflicts_version (name, rv, existing_version))
      else Ok ()

let rec check_all = function
  | [] -> Ok ()
  | c :: cs ->
      let open Result_O in
      let* () = c in
      check_all cs

let handle_conflicts packages conflicts =
  conflicts |> OpamFormula.to_disjunction
  |> List.map (handle_conflict packages)
  |> check_all

let handle_available available =
  if OpamFilter.eval_to_bool Env.common available then Ok ()
  else Error Available

let package_env version =
  Env.common
  |> Env.extend "version"
       (OpamVariable.string (OpamPackage.Version.to_string version))

let check_package packages env opam =
  check_all
    [
      opam |> OpamFile.OPAM.available |> handle_available;
      opam |> OpamFile.OPAM.depends
      |> OpamFilter.filter_formula env
      |> handle_depends packages;
      opam |> OpamFile.OPAM.conflicts
      |> OpamFilter.filter_formula env
      |> handle_conflicts packages;
    ]

module Conflicts_class_map : sig
  type t

  val empty : t
  val add_package : t -> OpamPackage.t -> OpamPackage.Name.t list -> t

  val fold :
    (OpamPackage.Name.t -> OpamPackage.t list -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  type t = OpamPackage.t list OpamPackage.Name.Map.t

  let empty = OpamPackage.Name.Map.empty

  let add_one t ~pkg ~cc =
    OpamPackage.Name.Map.union ( @ )
      (OpamPackage.Name.Map.singleton cc [ pkg ])
      t

  let add_package t pkg ccs =
    List.fold_left (fun acc cc -> add_one acc ~pkg ~cc) t ccs

  let fold f t z =
    OpamPackage.Name.Map.fold (fun cc pkgs acc -> f cc pkgs acc) t z
end

let apply repo packages =
  let package_map = to_version_map packages in
  let step1_map, conflicts_class_map =
    OpamPackage.Name.Map.fold
      (fun name version (acc_result_map, acc_conflicts_class_map) ->
        let pkg = OpamPackage.create name version in
        let env = package_env version in
        let opam = Repository.read_opam repo pkg in
        let result = check_package package_map env opam in
        ( OpamPackage.Map.add pkg result acc_result_map,
          Conflicts_class_map.add_package acc_conflicts_class_map pkg
            (OpamFile.OPAM.conflict_class opam) ))
      package_map
      (OpamPackage.Map.empty, Conflicts_class_map.empty)
  in
  Conflicts_class_map.fold
    (fun cc pkgs acc ->
      match pkgs with
      | [] -> assert false
      | [ _ ] -> acc
      | _ ->
          List.fold_left
            (fun acc pkg ->
              OpamPackage.Map.add pkg (Error (Conflict_class (cc, pkgs))) acc)
            acc pkgs)
    conflicts_class_map step1_map

let remove_packages_with_errors error_map =
  error_map
  |> OpamPackage.Map.filter (fun _pkg r -> Result.is_ok r)
  |> OpamPackage.Map.keys |> OpamPackage.Set.of_list
