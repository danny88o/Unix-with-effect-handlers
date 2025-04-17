open Effect
open Effect.Deep
open Printf

open State
open Helper


type 'a listmap = (int * 'a) list

let print_dir dir =
  List.iter (fun (k, v) -> printf "%s -> %i\n" k v) dir

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
      | Some fname_ino -> write fname_ino cs
      | None -> match (create fname) with
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
