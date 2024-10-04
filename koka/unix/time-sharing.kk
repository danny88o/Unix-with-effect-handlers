module time-sharing

/*
---------
Processes
---------
*/

effect fork
  ctl fork(): bool

pub fun nondet(action: () -> <fork|e> a): e list<a>
  with handler
    return(x) [x]
    ctl fork() resume(False) ++ resume(True)
  action()

effect interrupt
  val interrupt:()

type pState<a,e>
  Done(a:a)
  Paused(a:a, e:e)

// pub fun reifyProcess(action: () -> <pState|e> a): e pState<a>
//   with handler 
//     ctl done() return
//     ctl paused()  