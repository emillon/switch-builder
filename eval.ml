open Import

let rv (relop, version) existing_version =
  OpamFormula.eval_relop relop existing_version version

let rec m atom = function
  | OpamFormula.Empty -> Ok ()
  | Atom x -> atom x
  | Block x -> m atom x
  | And (x, y) ->
      let open Result_O in
      let* () = m atom x in
      m atom y
  | Or (x, y) -> ( match m atom x with Error _ -> m atom y | Ok () -> Ok ())
