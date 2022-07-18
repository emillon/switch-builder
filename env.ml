let common var =
  match OpamVariable.Full.to_string var with
  | "arch" -> Some (OpamVariable.string "x86_64")
  | "os" -> Some (OpamVariable.string "linux")
  | "os-distribution" -> Some (OpamVariable.string "debian")
  | "os-family" -> Some (OpamVariable.string "debian")
  | "enable-ocaml-beta-repository" -> Some (OpamVariable.bool false)
  | "opam-version" -> Some (OpamVariable.string "2.1.0")
  | "sys-ocaml-version" -> Some (OpamVariable.string "4.14.0")
  | _ ->
      OpamFilter.deps_var_env ~build:true ~post:false ~test:true ~doc:false
        ~dev:false var

let extend k v env var =
  if String.equal (OpamVariable.Full.to_string var) k then Some v else env var
