open Effect
open Effect.Deep
open Effect.Shallow

open Fs
open Helper

type _ Effect.t += Yield : string -> unit Effect.t
                 | Await : string Effect.t

let yield (x) = perform (Yield x)
let await () = perform Await

let (||) (i: unit -> unit) (o: unit-> unit): unit -> unit =
  let rec pipe: type a. (unit, unit) continuation -> (a, unit) continuation -> a -> unit = fun p c v ->
    continue_with c v {
      retc = Fun.id;
      exnc = raise;
      effc = fun (type b) (eff : b Effect.t) ->
        match eff with
        | Await -> Some (fun (k: (b,_) continuation) ->
          copipe k p)
        | _ -> None
    }
  and copipe: (string, unit) continuation -> (unit, unit) continuation -> unit = fun c p ->
    continue_with p () {
      retc = Fun.id;
      exnc = raise;
      effc = fun (type b) (eff : b Effect.t) ->
        match eff with
        | Yield v -> Some (fun (k: (b,_) continuation) ->
          pipe k c v)
        | _ -> None
    }
  in
  let l () =
    try pipe (fiber i) (fiber o) () with
      | effect (Yield v), k -> continue k (write 0 v)
  in l

let cat (fname: string): unit =
  match open_file(fname) with
    | None -> exit(1)
    | Some ino -> match read(ino) with
      | None -> exit(2)
      | Some cs ->
        let go (x: char) = yield (String.make 1 x) in
         String.iter go cs;
         go('0')

let rec head (n: int): unit =
  if n == 0 then yield("0")
  else
    let c = await() in
    yield(c);
    match c with
    | "0" -> ()
    | "\n" -> head(n - 1)
    | _ -> head(n)

let paste() =
    let rec pst(c: string) (cs:string) =
      match c with
        | "0"  -> yield(cs); yield("0")
        | "\n" -> yield(cs); yield("\n"); pst (await()) " "
        | " "  -> yield(cs); pst (await()) " "
        | _    -> pst (await())  (cs ^ c)
    in
    pst (await()) ""

let rec sed (target: string) (replacement: string): unit =
  let str = await() in
  if str = target then yield(replacement) else yield(str);
  sed target replacement

let freq (): unit =
  let renderRow (tuple: (string * int)) =
    let (s, i) = tuple in s ^ ":" ^ (string_of_int i) ^ ";"
  in
  let renderTable (tbl: (string * int) list) =
    let row = List.map renderRow tbl in String.concat " " row
  in
  let rec freq' (str: string) (tbl: (string * int) list) =
    match str with
      | "0" -> yield(renderTable tbl)
      | _   ->
        let sum (): int = lookup_fail str tbl in
        let modi () = modify str (sum() + 1) tbl in
        let tbl' = withDefault ((str, 1) :: tbl) modi in
        freq' (await()) tbl'
  in
  freq' (await()) []

