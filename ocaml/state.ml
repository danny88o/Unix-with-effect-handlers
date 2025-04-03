open Effect
open Effect.Deep


type _ Effect.t += Xchg: int -> int t

let comp1 () = perform (Xchg 0) + perform (Xchg 1)


let run1 () =
  try comp1 () with
  | effect (Xchg n), k -> continue k (n+1)

let run2(main:unit -> int):int =
  let loop () =
    try main () with
    | effect (Xchg n), k -> continue k (n+1)
  in
  loop ()


let () = print_endline (string_of_int (run2 (comp1)));;



module State (S: sig type t end) = struct
  type _ Effect.t += Get: unit -> S.t t
                   | Put: S.t -> unit t

  let get () = perform (Get ())
  let put x = perform (Put x)

  let run (initial:S.t) (m: unit -> 'a): ('a * S.t) =
    let state = ref initial in
    try
      (m (), !state)
    with
    | effect (Get ()), k ->
      continue k !state
    | effect (Put x), k ->
      state := x;
      continue k ()
end

module IntState = State (struct type t = int end)

let _ =
  IntState.run 0 (fun () ->
    IntState.put 42;
    IntState.put 21;
    print_endline (string_of_int (IntState.get ()))
  )


