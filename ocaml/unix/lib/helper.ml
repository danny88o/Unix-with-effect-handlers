open Effect
open Effect.Deep

exception Unwind
let finalise(k) = try ignore (discontinue k Unwind) with _ -> ()

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
  | (k,_)::l -> if k = key then true else has key l
