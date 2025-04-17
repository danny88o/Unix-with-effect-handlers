(* open Unix.Scheduler
open Unix.Fs
open Unix.Pipe *)
open Unix.Session

open Printf


(*
let _ =
  let (_, fs) =
    FS_State.run initial_fs ( fun () ->
      fileIO (fun () ->
        let hello () = echo "Hello, " in
        let world () = echo "World" in
        let header () = head(1) in
        let meaow () = cat("myfile") in
        hello > "myfile" ;
        world >> "myfile" ;
        (meaow || header) >> "myfile2";
      ))
  in
  print_dir fs.dir;
  print_dreg fs.dreg;
  printf "End of FS\n";
;;


let _ =
  let run () = scheduler initial_state_s fork_eg in
  match run() with
  | [] -> printf "No processes\n"
  | ((_,v) :: _) -> printf "Result: %i\n" v
;;



let _ =
  printf "File System Example\n";
  let (_, fs) =
  FS_State.run initial_fs (fun () ->
  fileIO (fun () ->
    let hello () = echo "Hello, " in
    let world () = echo "World" in
    hello > "myfile" ;
    world >> "myfile" ;
    hello ();
    world ();
    copy true "myfile" "myfile2" ;
    remove "myfile" ;
    copy false "myfile2" "myfile3";
  ))
  in
  print_dir fs.dir;
  print_dreg fs.dreg;
  printf "End of FS\n";

;; *)

let _ =
  let a =
    sessionmgr2 Root (fun () ->
      printf "%s\n" (whoami());
      switchUser(Alice);
      printf "%s\n" (whoami());
      switchUser Bob;
      whoami()
    )
  in
  printf "%s\n" a;
;;
