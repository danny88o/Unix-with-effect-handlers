open Unix.Fs
open Sys

let ((), fs) =
  let n = Sys.argv.(1) |> int_of_string in
  FS_State.run initial_fs (fun () ->
    fileIO (fun () ->
      for i = 1 to n do
        let hello () = echo "Hello, " in
        let world () = echo "World" in
        hello > "myfile" ;
        world >> "myfile" ;
        hello ();
        world ();
        copy true "myfile" "myfile2" ;
        remove "myfile" ;
        copy false "myfile2" "myfile3" ;
      done))



