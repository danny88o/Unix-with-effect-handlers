open Unix.Scheduler

let _ =
  let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | _ ->
    let pid1 = fork (fun () -> fib (n-1)) in
    let pid2 = fork (fun () -> fib (n-2)) in
    let () = uInterrupt () in
    let a = wait pid1 in
    let b = wait pid2 in
    a + b
  in
  let n = Sys.argv.(1) |> int_of_string in
  fib n