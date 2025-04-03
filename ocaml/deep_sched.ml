open Effect
open Effect.Deep

module type TYPE = sig
  type t
end

(* [STATE] is the type of a module that offers the functions [get] and [set]
   for manipulating a piece of mutable state with contents in the type [t].
   This module must also offer a function [run] for handling computations
   that perform the operations [get] and [set].
*)
module type STATE = sig
  type t

  val get : unit -> t
  val set : t -> unit
  val run : init:t -> (unit -> 'a) -> t * 'a
end

(* [CELL] is the type of a functor that produces an
   implementation of [STATE] for any given type.
*)
module type CELL = functor (T : TYPE) -> STATE with type t = T.t

module GlobalMutVar : CELL =
functor
  (T : TYPE)
  ->
  struct
    type t = T.t

    let var = ref None
    let get () = match !var with Some x -> x | None -> assert false
    let set y = var := Some y

    let run ~init main =
      set init |> fun _ ->
      main () |> fun res ->
      get () |> fun x ->
      (var := None) |> fun _ -> (x, res)
  end

open Printf

module IntCell = GlobalMutVar (struct
  type t = int
end)

module StrCell = GlobalMutVar (struct
  type t = string
end)

let main () : unit =
  IntCell.(
    printf "%d\n" (get ());
    set 42;
    printf "%d\n" (get ());
    set 21;
    printf "%d\n" (get ());

    StrCell.(
      set "Hello...";
      printf "%s\n" (get ());
      set "...World!";
      printf "%s\n" (get ());
      )

    );




type process =
  | Ready of {id: int; resumption: (unit -> unit)}
  | Blocked of {id: int; resumption: (int -> unit); waiting: int}

type 'a pState = {
  q: process Queue.t;
  d: (int * 'a) List.t;
  pid: int;
  pnext: int;
}

let initial_state : 'a pState = {
  q = Queue.create ();  (* Create an empty queue *)
  d = [];               (* Empty list for completed processes *)
  pid = 1;              (* Starting process ID *)
  pnext = 2;            (* Next process ID to assign *)
}

type _ Effect.t += Fork  : (unit -> unit) -> int t
| Wait : int -> unit t
| UInterrupt : unit t
| Pid: int t


let fork f = Effect.perform (Fork f)
let uInterrupt () = Effect.perform UInterrupt
let wait pid = Effect.perform (Wait pid)
let get_pid () = Effect.perform Pid


let sched action =
  let state = ref initial_state
  in
  let runNext () =
    let st = !state in
    match Queue.take_opt st.q with
    | None -> ()
    | Some(Ready({id; resumption})) ->
      state := {st with pid=id};
      resumption ()
    | Some(Blocked({id; resumption; waiting})) ->
      state := {st with pid=id};
      resumption id (*Very Bad!!!! TODO*)
  in
  match
  action ();
  with
  | effect (Fork f), k ->
    let st = !state in
    let child_pid = st.pnext in
    let p = Ready({id=child_pid; resumption=f}) in
    Queue.push p st.q;
    state := {st with pnext=child_pid+1};
    continue k child_pid

  | effect (UInterrupt), k  ->
    let st = !state in
    let res () = continue k () in
    let p = Ready({id=st.pid; resumption=res}) in
    runNext (Queue.push p st.q)

  | effect (Pid), k -> continue k (!state).pid

  | a -> runNext ();
