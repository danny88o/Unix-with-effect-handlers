open Unix.Session

let rec repeat n f =
  if n > 0 then (f (); repeat (n-1) f)
let _ =
  let n = Sys.argv.(1) |> int_of_string in
  sessionmgr2 Root ( fun () ->
  repeat n (fun () ->
    match whoami() with
    | "root" -> switchUser (Alice)
    | "alice" -> switchUser (Bob)
    | "bob" -> switchUser (Root)
    | _ -> ()
    ))