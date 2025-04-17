open Effect
(* open Effect.Deep *)

open Helper

type _ Effect.t += Exit: int -> 'a t

let exit n = perform (Exit n)



let exitHandler(main: unit -> 'a): int =
  let run () =
    try
    ignore (main ());
    0
    with
    | effect (Exit n), k -> finalise k; n;
  in
  run ()