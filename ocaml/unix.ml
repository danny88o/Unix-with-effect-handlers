open Effect
open Effect.Deep
open Effect.Shallow
open Printf


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

type _ Effect.t += Exit: int -> 'a t

let exit n = perform (Exit n)

exception Unwind
let finalise(k) = try ignore (discontinue k Unwind) with _ -> ()

let exitHandler(main: unit -> 'a): int =
  let run () =
    try
    ignore (main ());
    0
    with
    | effect (Exit n), k -> finalise k; n;
  in
  run ()


let example () = if true then exit 4 else "3"

(* let _ = printf "%i\n" (
  exitHandler
    example
  ) *)

type _ Effect.t += Fail: unit -> 'a t

let fail () = perform (Fail ())

let withDefault (default: 'a) (action : unit -> 'a): 'a =
  let run () =
    try
      action ()
    with
    | effect (Fail ()), k -> finalise k; default
  in
  run ()

let rec lookup_fail key = function
  | [] -> fail ()
  | (k,v)::l -> if k = key then v else lookup_fail key l

let rec modify key value = function
  | [] -> fail ()
  | (k,v)::l -> if k = key then (k, value)::l else (k,v)::(modify key value l)

let rec remove key = function
  | [] -> fail ()
  | (k,v)::l -> if k = key then l else (k,v)::(remove key l)
let rec lookup key = function
  | [] -> None
  | (k,v)::l -> if k = key then Some v else lookup key l

let rec has key = function
  | [] -> false
  | (k,v)::l -> if k = key then true else has key l

(*

FILE SYSTEM

*)

type 'a listmap = (int * 'a) list

let print_dir dir =
  List.iter (fun (k, v) -> printf "%s -> %i\n" k v) dir

(* let print_iList ilist =
  List.iter (fun (k, v) -> printf "%i -> %i\n" k v.lno) ilist *)

let print_dreg dreg =
  List.iter (fun (k, v) -> printf "%i -> %s\n" k v) dreg

type inode = {lno: int; loc:int}

type file_system = {
  dir: (string * int) list;
  iList: inode listmap;
  dreg: string listmap;
  dnext: int;
  inext: int;
}

let fread fs ino =
  let inode = lookup_fail ino fs.iList in
  lookup_fail inode.loc fs.dreg
let fwrite fs ino (cs: string) =
  let inode = lookup_fail ino fs.iList in
  let loc = inode.loc in
  let file = lookup_fail loc fs.dreg in
  let dreg = modify loc (file ^ cs) fs.dreg in
  {fs with dreg=dreg}

let fopen fs fname =
  lookup_fail fname fs.dir

let fcreate fs fname =

  if has fname fs.dir then
    let ino = fopen fs fname in
    let inode = lookup_fail ino fs.iList in
    let dreg' = modify inode.loc "" fs.dreg in
    (ino, {fs with dreg=dreg'})
  else
    let loc = fs.dnext in
    let dreg' = (loc, "") :: fs.dreg in
    let ino = fs.inext in
    let inode = {lno=1; loc=loc} in
    let ilist' = (ino, inode) :: fs.iList in
    let dir' = (fname, ino) :: fs.dir in
    (ino, {dir=dir'; iList=ilist'; dreg=dreg'; dnext=fs.dnext+1; inext=fs.inext+1})

let flink fs src dest =
  if has dest fs.dir then
    fail ()
  else
    let ino = lookup_fail src fs.dir in
    let inode = lookup_fail ino fs.iList in
    let inode' = {inode with lno=inode.lno+1} in
    let dir' = (dest, ino) :: fs.dir in
    let ilist' = modify ino inode' fs.iList in
    {fs with dir=dir'; iList=ilist'}

let funlink fs fname =
  let ino = lookup_fail fname fs.dir in
  let inode = lookup_fail ino fs.iList in
  let dir' = remove fname fs.dir in
  if inode.lno > 1 then
    let inode' = {inode with lno=inode.lno-1} in
    let ilist' = modify ino inode' fs.iList in
    {fs with dir=dir'; iList=ilist';}
  else
    let ilist' = remove ino fs.iList in
    let dreg' = remove inode.loc fs.dreg in
    {fs with dir=dir'; iList=ilist'; dreg=dreg';}


module FS_State = State (struct type t = file_system end)

type _ Effect.t +=  Read: int ->  string option t
                  | Write: int * string -> unit t
                  | Create: string -> int option t
                  | Open: string -> int option t
                  | Link: string * string -> unit t
                  | Unlink: string -> unit t

let read inum = perform (Read inum)
let write inum cs = perform (Write (inum, cs))
let create fname = perform (Create fname)
let open_file fname = perform (Open fname)
let link src dest = perform (Link (src, dest))
let remove fname = perform (Unlink fname)

let initial_fs: file_system = {
  dir = [("stdout", 0)];
  iList = [(0, {lno=1; loc=0})];
  dreg = [(0, "")];
  dnext = 1;
  inext = 1;
}

let echo cs = perform (Write (0, cs))

let fileIO (action: unit -> 'a): ('a) =
    try
    action ()
    with
    | effect (Read inum), k ->
      let v = withDefault None (fun () -> Some (
        let fs = FS_State.get () in fread fs inum
      )) in
      continue k v

    | effect (Write (inum, cs)), k ->
      withDefault () (fun () -> (
        let fs = FS_State.get () in
        let fs' = fwrite fs inum cs in
        FS_State.put fs';
      ));
      continue k ()
    | effect (Create fname), k ->
      let v = withDefault None (fun () -> Some (
        let fs = FS_State.get () in
        let (ino, fs') = fcreate fs fname in
        FS_State.put fs';
        ino
      )) in
      continue k v
    | effect (Open fname), k ->
      let v = withDefault None (fun () -> Some (
        let fs = FS_State.get () in
        let ino = fopen fs fname in
        ino
      )) in
      continue k v
    | effect (Link (src, dest)), k ->
      withDefault () (fun () -> (
        let fs = FS_State.get () in
        let fs' = flink fs src dest in
        FS_State.put fs';
      ));
      continue k ()
    | effect (Unlink fname), k ->
      withDefault () (fun () -> (
        let fs = FS_State.get () in
        let fs' = funlink fs fname in
        FS_State.put fs';
      ));
      continue k ()

let (>) (action: unit -> unit) (fname: string) =
  match
  action ()
  with
  | () -> ()
  | effect (Write (inum, cs)), k ->
    if inum = 0 then match (create fname) with
      | None -> exit 1
      | Some fname_ino -> write fname_ino cs
    else
      write inum cs;
    continue k ()

let (>>) (action: unit -> unit) (fname: string) =
  match
  action ()
  with
  | () -> ()
  | effect (Write (inum, cs)), k ->
    if inum = 0 then match (open_file fname) with
      | None -> exit 2
      | Some fname_ino -> write fname_ino cs
    else
      write inum cs;
    continue k ()

let copy (shallow: bool) (src: string) (dest: string) =
  if shallow then link src dest
  else
    let ino = match open_file src with
    | None -> exit 3
    | Some ino -> ino
    in
    let file = match read ino with
      | None -> exit 4
      | Some file -> file
    in
    let stream () = echo file in stream > dest


let _ =
  printf "File System Example\n";
  let (fs_get, fs) =
  FS_State.run initial_fs ( fun () ->
    fileIO (fun () ->
      (*Rewriet Imperative style*)
      let hello () = echo "Hello, " in
      let world () = echo "World" in
      hello > "myfile" ;
      world >> "myfile" ;
      hello ();
      world ();
      copy true "myfile" "myfile2" ;
      remove "myfile" ;
      copy false "myfile2" "myfile3" ;

  ))
  in
  print_dir fs.dir;
  print_dreg fs.dreg;
  printf "End of FS\n";

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
  | {input; output} -> {q with input=x::input}
let rec dequeue q = match q with
  | {input = []; output = []} -> None
  | {input; output = []} -> dequeue {input=[]; output=List.rev input}
  | {input; output = x::xs } -> Some (x, {q with output=xs})

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

let _ =
  let run () = scheduler initial_state_s fork_eg in
  match run() with
  | [] -> printf "No processes\n"
  | ((k,v) :: xs) -> printf "Result: %i\n" v;
