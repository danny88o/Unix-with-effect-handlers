module coop

import basic
import helper

/*
Cooperative Multithreading
*/
pub effect coop<a>
  ctl yield(): ()
  fun spawn(f: () -> ()): ()
  // ctl wait(pid: int): a

alias proc<a,e> = () -> ()

type stateP<a,e>
  Blocked(pid:int, res:proc<a,e>)
  Ready(res: proc<a,e>)

struct stateS<a,e> (
  q:list<(int,stateP<a,e>)> = [],
  done:list<(int,a)> = [],
  pid:int = 1,
  pnext:int = 2
)

pub fun multithreading(action:() -> <coop<b>,div|e> a): <div|e> ()
  var state := StateS()

  fun enqueue(t)
    val pnext = state.pnext
    val q = state.q ++ [(pnext, Ready(t))]
    state := state(q=q, pnext=pnext+1)

  fun dequeue()
    match state.q
      Nil -> ()
      Cons((pid, Ready(t)), ts) ->
            state := state(q=ts, pid=pid)
            t()
      Cons((pid, Blocked(pid', t)), ts) ->
            if state.done.has(pid') then
              state := state(q=ts, pid=pid)
            else
              state := state(q=ts ++ [(pid, Blocked(pid', t))])
              dequeue()

  fun hold(t)
    val q = state.q ++ [(state.pid, Ready(t))]
    state := state(q=q)

  with handler
    return(x)
      val done = state.done ++ [(state.pid, x)]
      state := state(done=done)
      dequeue()
    fun spawn(t)  enqueue(t)
    ctl yield()  {hold(fn() resume(())); dequeue()}

  action()

// pub fun fib(n: int): <coop|e> int
//   spawn(write(1,"2"))
//   1


