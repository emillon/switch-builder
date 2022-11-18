open Import

let depends_on_any packages (formula : OpamTypes.filtered_formula) =
  let packages = List.map OpamPackage.Name.of_string packages in
  let is_one_of_packages name =
    List.exists (fun p -> OpamPackage.Name.compare p name = 0) packages
  in
  OpamFormula.fold_left
    (fun acc (name, _) -> acc || is_one_of_packages name)
    false formula

let depends_on_dune ~allow_jbuilder (formula : OpamTypes.filtered_formula) =
  let packages =
    if allow_jbuilder then [ "dune"; "jbuilder" ] else [ "dune" ]
  in
  depends_on_any packages formula

let is_dune package =
  OpamPackage.Name.equal (OpamPackage.name package)
    (OpamPackage.Name.of_string "dune")

let is_compiler opam =
  let ocaml_core_compiler = OpamPackage.Name.of_string "ocaml-core-compiler" in
  List.exists
    (fun cc -> OpamPackage.Name.equal ocaml_core_compiler cc)
    (OpamFile.OPAM.conflict_class opam)

let is_virtual opam = Option.is_none (OpamFile.OPAM.url opam)

let is_compatible repo package =
  let opam = Repository.read_opam repo package in
  let depends = OpamFile.OPAM.depends opam in
  let depopts = OpamFile.OPAM.depopts opam in
  is_dune package || is_compiler opam || is_virtual opam
  || depends_on_dune ~allow_jbuilder:true depends
  || depends_on_dune ~allow_jbuilder:true depopts

type t = { opam : OpamPackage.Set.t; workspace : OpamPackage.Set.t }

let equal { opam; workspace } t =
  OpamPackage.Set.equal opam t.opam
  && OpamPackage.Set.equal workspace t.workspace

let analyze repo packages =
  let workspace, opam =
    OpamPackage.Set.partition (fun pkg -> is_compatible repo pkg) packages
  in
  let g = packages |> to_version_map |> Package_graph.load repo in
  fixpoint ~equal
    ~f:(fun { opam; workspace } ->
      let r =
        Package_graph.fold_edges
          (fun pkg dependency acc ->
            if
              OpamPackage.Set.mem pkg opam
              && OpamPackage.Set.mem dependency workspace
            then OpamPackage.Set.add dependency acc
            else acc)
          g OpamPackage.Set.empty
      in
      {
        opam = OpamPackage.Set.union opam r;
        workspace = OpamPackage.Set.diff workspace r;
      })
    { workspace; opam }

let pp ppf { opam; workspace } =
  Format.fprintf ppf "Opam switch:\n%aWorkspace:\n%a" pp_package_set opam
    pp_package_set workspace

let rec increment_least_significant = function
  | [] -> None
  | x :: xs -> (
      match increment_least_significant xs with
      | Some segments -> Some (x :: segments)
      | None -> (
          match int_of_string_opt x with
          | Some n -> Some (string_of_int (Int.succ n) :: xs)
          | None -> None))

let next_version version =
  let segments =
    version |> OpamPackage.Version.to_string |> String.split_on_char '.'
  in
  match increment_least_significant segments with
  | None -> None
  | Some segments ->
      segments |> String.concat "." |> OpamPackage.Version.of_string |> fun x ->
      Some x

let opam_file_depends pkg : OpamTypes.filtered_formula =
  let name = OpamPackage.name pkg in
  let version = OpamPackage.version pkg in
  match next_version version with
  | None ->
      Atom
        ( name,
          Atom
            (Constraint (`Eq, FString (OpamPackage.Version.to_string version)))
        )
  | Some next_version ->
      Atom
        ( name,
          And
            ( Atom
                (Constraint
                   (`Geq, FString (OpamPackage.Version.to_string version))),
              Atom
                (Constraint
                   (`Lt, FString (OpamPackage.Version.to_string next_version)))
            ) )

let opam_file_x_provided pkgs =
  OpamTypesBase.nullify_pos
    (OpamParserTypes.FullPos.List
       (OpamTypesBase.nullify_pos
          (List.map
             (fun pkg ->
               OpamTypesBase.nullify_pos
                 (OpamParserTypes.FullPos.String
                    (OpamPackage.name pkg |> OpamPackage.Name.to_string)))
             (OpamPackage.Set.elements pkgs))))

let opam_file { opam; workspace } =
  let depends =
    OpamPackage.Set.union opam workspace
    |> OpamPackage.Set.elements |> List.map opam_file_depends
    |> OpamFormula.ands
  in
  OpamFile.OPAM.empty
  |> OpamFile.OPAM.with_depends depends
  |> OpamFile.OPAM.with_extensions
       (OpamStd.String.Map.of_list
          [ ("x-opam-monorepo-opam-provided", opam_file_x_provided opam) ])
