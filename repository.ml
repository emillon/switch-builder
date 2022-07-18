open Import

module Path = struct
  type t = OpamTypes.dirname

  let create = OpamFilename.Dir.of_string

  let fold_subdirs dir ~f z =
    let contents = Sys.readdir dir in
    Array.fold_left
      (fun acc s ->
        if String.starts_with ~prefix:"." s then acc
        else
          let subdir = Filename.concat dir s in
          if Sys.is_directory subdir then f subdir ~base:s acc else acc)
      z contents

  let packages dir =
    let packages_dir =
      Filename.concat (OpamFilename.Dir.to_string dir) "packages"
    in
    fold_subdirs packages_dir
      ~f:(fun package_dir ~base acc ->
        let name = OpamPackage.Name.of_string base in
        let versions =
          fold_subdirs package_dir
            ~f:(fun package_version_dir ~base acc ->
              if Sys.file_exists (Filename.concat package_version_dir "opam")
              then
                let package = OpamPackage.of_string base in
                let version = OpamPackage.version package in
                OpamPackage.Version.Set.add version acc
              else acc)
            OpamPackage.Version.Set.empty
        in
        OpamPackage.Name.Map.add name versions acc)
      OpamPackage.Name.Map.empty

  let path root pkg =
    let prefix = Some (OpamPackage.name_to_string pkg) in
    OpamRepositoryPath.opam root prefix pkg

  let read_opam root pkg = OpamFile.OPAM.read (path root pkg)
end

module Stats = struct
  type t = (OpamPackage.t, int) Hashtbl.t

  let create () = Hashtbl.create 0

  let pp ppf t =
    Hashtbl.iter (fun pkg n -> Format.fprintf ppf "%a: %d\n" pp_package pkg n) t

  let record t pkg =
    let c = Hashtbl.find_opt t pkg |> Option.value ~default:0 in
    Hashtbl.replace t pkg (c + 1)
end

module Cache = struct
  type t = (OpamPackage.t, OpamFile.OPAM.t) Hashtbl.t

  let create () = Hashtbl.create 0

  let with_ h pkg f =
    match Hashtbl.find_opt h pkg with
    | Some r -> r
    | None ->
        let r = f () in
        Hashtbl.replace h pkg r;
        r
end

type t =
  | Path of Path.t
  | Combine of t * t
  | Stats of Stats.t * t
  | Cache of Cache.t * t

let path s = Path (Path.create s)
let stats s t = Stats (s, t)
let cache c t = Cache (c, t)
let combine a b = Combine (a, b)

let rec packages = function
  | Path root -> Path.packages root
  | Combine (a, b) ->
      OpamPackage.Name.Map.union OpamPackage.Version.Set.union (packages a)
        (packages b)
  | Stats (_, t) -> packages t
  | Cache (_, t) -> packages t

let rec mem t pkg =
  match t with
  | Path root -> OpamFile.exists (Path.path root pkg)
  | Combine (a, b) -> mem a pkg || mem b pkg
  | Stats (_, t) -> mem t pkg
  | Cache (_, t) -> mem t pkg

let rec read_opam t pkg =
  match t with
  | Path root -> Path.read_opam root pkg
  | Combine (a, b) -> if mem a pkg then read_opam a pkg else read_opam b pkg
  | Stats (h, t) ->
      Stats.record h pkg;
      read_opam t pkg
  | Cache (h, t) -> Cache.with_ h pkg (fun () -> read_opam t pkg)
