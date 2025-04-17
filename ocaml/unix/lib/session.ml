open Effect
open Effect.Deep
open Effect.Shallow

type user =
  | Alice
  | Bob
  | Root

let to_string (user) = match user with
  | Alice -> "alice"
  | Bob   -> "bob"
  | Root  -> "root"

type _ Effect.t += Ask : string Effect.t
                 | SubUser : user -> unit Effect.t

let whoami () = perform Ask
let switchUser (user) = perform (SubUser user)

let sessionmgr (u: user) (action: unit -> 'a): 'a =
  let env curr run =
    try run () with
    | effect Ask, k -> continue k (curr |> to_string)
  in
  let rec handle: (unit,'a) continuation -> 'a = fun k ->
    continue_with k () {
      retc = Fun.id;
      exnc = raise;
      effc = fun (type b) (eff : b Effect.t) ->
        match eff with
        | SubUser u_new -> Some (fun (k: (b,_) continuation) ->
          env u_new (fun () -> handle k)
          )
        | _ -> None
    }
  in
  env u (fun () -> handle (fiber action))

let sessionmgr2 (u: user) (action: unit -> 'a): 'a =
  let curr = ref u in
  try action() with
    | effect Ask, k             -> continue k (!curr |> to_string)
    | effect (SubUser new_u), k -> continue k (curr := new_u)


