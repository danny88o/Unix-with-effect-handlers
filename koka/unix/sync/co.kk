module sync/co

import basic

pub effect co<a>
  ctl uFork(): int
  ctl wait(pid: int): a
  ctl uInterrupt(): ()
  ctl getPid():int

pub fun retro-sync(action: () -> <interrupt, fork, co<a>| e> a): <co<a>|e> a
  with fun fork() uFork() > 0
  with fun interrupt() uInterrupt()
  action()


pub fun init(main: () -> <co<a>|e> ()): <co<a>|e> ()
  val pid = uFork()
  if pid == 0
  then
    main()
  else
    wait(pid)
    ()



// pub alias funcqueue<a> = (list<a>, list<a>)

// pub fip(1) fun enqueue(q:funcqueue<a>, x:a): funcqueue<a>
//   match q
//     (in_q, out_q) -> (Cons(x,in_q), out_q)


// pub fbip fun dequeue(q:funcqueue<a>): div maybe<(a, funcqueue<a>)>
//   match q
//     (Nil, Nil) -> Nothing
//     (in_q, Nil) -> dequeue((Nil, in_q.reverse()))
//     (in_q, Cons(x, xs)) -> Just((x, (in_q, xs)))

// pub fip fun to-back(q:funcqueue<a>): div funcqueue<a>
//   match q
//     (Nil, Nil) -> (Nil, Nil)
//     (in_q, Nil) -> to-back((Nil, in_q.reverse()))
//     (in_q, Cons(x, xs)) -> (Cons(x, in_q), xs)
