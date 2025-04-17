open Effect
open Effect.Shallow

open Helper

(*

SCHEDULER

*)

type _ Effect.t +=
    Fork       : (unit -> int) -> int t
  | Wait       : int -> int t
  | UInterrupt : unit t
  | Pid        : int t


let fork f = Effect.perform (Fork f)
let uInterrupt () = Effect.perform UInterrupt
let wait pid = Effect.perform (Wait pid)
let get_pid () = Effect.perform Pid

type 'a funcqueue = {input: 'a list; output: 'a list}
let empty_queue = {input=[]; output=[]}

let enqueue x q = match q with
  | {input; _} -> {q with input=x::input}
let rec dequeue q = match q with
  | {input = [];    output = []} -> None
  | {input = input; output = []} -> dequeue {input=[]; output=List.rev input}
  | {input = _;     output = x::xs } -> Some (x, {q with output=xs})

type 'a process_s =
  | Ready of {id: int; resumption: (unit, 'a) Shallow.continuation}
  | Blocked of {id: int; resumption: (int, 'a) Shallow.continuation; waiting: int}

type 'a pState_s = {
  q: 'a process_s funcqueue;
  d: (int * 'a) List.t;
  pid: int;
  pnext: int;
}

let initial_state_s: int pState_s = {
  q = empty_queue;
  d = [];
  pid = 1;
  pnext = 2;
}

let rec print_list = function
  [] -> ()
  | (a,b)::l -> print_string "("; print_int a ; print_string ","; print_int b; print_string "), " ; print_list l


let scheduler (init_state: int pState_s) (action: unit -> int): (int * int) list =
  let rec runNext st = match dequeue st.q with
    | None -> st.d
    | Some(Ready({id; resumption}), q') ->
      sched resumption () {st with pid=id; q=q'}
    | Some(Blocked({id; resumption; waiting}), q') ->
      match lookup waiting st.d with
      | None ->
        let p = Blocked({id=id; resumption=resumption; waiting=waiting}) in
        runNext {st with q= enqueue p q'}
      | Some a -> sched resumption a {st with pid=id; q=q'}

  and sched: type a. (a, int) continuation -> a -> int pState_s -> (int * int) list = fun k v state ->
    continue_with k v {
      retc = (fun a -> let d = (state.pid, a) :: state.d in runNext {state with d});
      exnc = raise;
      effc = fun (type b) (eff : b Effect.t) -> match eff with
        | Fork f -> Some (fun (k: (b,_) continuation) ->
          let child_pid = state.pnext in
          let p = Ready({id=child_pid; resumption=fiber f}) in
          let q = enqueue p state.q in
          sched k child_pid {state with q=q; pnext=child_pid+1})

        | Wait pid -> Some (fun (k: (b,_) continuation) ->
          let p = Blocked({id=state.pid; resumption=k; waiting=pid}) in
          let q = enqueue p state.q in
          runNext {state with q=q;}
          )

        | UInterrupt -> Some (fun (k: (b,_) continuation) ->
          let p = Ready({id=state.pid; resumption=k}) in
          let q = enqueue p state.q in
          runNext {state with q=q;}
          )

        | Pid -> Some (fun (k: (b,_) continuation) ->
          sched k state.pid state
        )

        | _ -> None}
  in
  sched (fiber action) () init_state

let fork_eg () =
  let rec fib n = match n with
    | 0 -> 0
    | 1 -> 1
    | v ->
      let pid1 = fork (fun () -> fib (v-1)) in
      let pid2 = fork (fun () -> fib (v-2)) in
      let () = uInterrupt () in
      let a = wait pid1 in
      let b = wait pid2 in
      a + b
  in
  fib 20
