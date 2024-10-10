module time-sharing

import io
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

pub fun reifyProcess(action: () -> <interrupt|e> a): e pState<a,e>
  with raw ctl interrupt() 
      Paused(fn() rcontext.resume(()))
  Done(action())

pub fun schedule(jobs: list<pState<a,<fork,div|e>>>): <div|e> list<a>
  fun sched(ps: list<pState<a,<fork,div|e>>>, done: list<a>): <div|e> list<a>
    match ps
      Nil -> done
      Cons(x,xs) -> match x
        Done(ret) -> sched(xs, done ++ [ret])
        Paused(action) -> sched(xs ++ nondet(action), done) 
  sched(jobs, [])

pub fun timeshare(action: () -> <fork,interrupt,div|e> a): <div|e> list<a>
  schedule([Paused(fn () reifyProcess(action))])

pub fun interruptWrite(action: () -> <bio,interrupt|e> a): <bio,interrupt|e> a
  with fun write(fd,s)
    interrupt()
    write(fd,s)
  mask<bio>
    action()