open Unix.Session

let _ =
  let n = Sys.argv.(1) |> int_of_string in
  sessionmgr Root ( fun () ->
    for i = 1 to n do
      match whoami() with
      | "root" -> switchUser (Alice)
      | "alice" -> switchUser (Bob)
      | "bob" -> switchUser (Root)
      | _ -> ()
    done;
    )