module time-sharing

/*
---------
Processes
---------
*/

effect fork
  ctl fork(): bool

pub fun do_fork(): fork bool
  fork()

pub fun nondet(action: () -> <fork|e> a): e list<a>
  with handler
    return(x) [x]
    ctl fork() resume(True) ++ resume(False)
  action()


effect interrupt
  ctl interrupt():()

fun do_interrupt(): interrupt ()
  interrupt()

type pState<a,e>
  Done(ret: a)
  Paused(remaction: () -> e pState<a,e>)

type action<specific,general>
  Action

pub fun reifyProcess(action: () -> <interrupt|e> a): e pState<a,e>
  with raw ctl interrupt() 
      Paused(fn() rcontext.resume(()))
  Done(action())

pub fun schedule<a>(actions: list<pState<a,e>>): e list<a>
  var done := []
  fun sched(ax: list<pState<a,e>>): e list<a>
    match ax
      Nil -> done
      Cons(x,xs) -> match x
        Done(ret) -> {
          done := done ++ [ret]
          sched(xs)
        }
        Paused(action) -> sched(xs ++ nondet(action)) 
  sched(actions)