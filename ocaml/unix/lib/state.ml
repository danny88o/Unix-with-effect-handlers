open Effect
open Effect.Deep

module State (S: sig type t end) = struct
  type _ Effect.t += Get: unit -> S.t t
                   | Put: S.t -> unit t

  let get () = perform (Get ())
  let put x = perform (Put x)

  let run (initial:S.t) (m: unit -> 'a): ('a * S.t) =
    let state = ref initial in
    match
    m ()
    with
    | v -> (v, !state)
    | effect (Get ()), k -> continue k !state
    | effect (Put x), k -> state := x; continue k ()
end