module sync/parametrized

import basic
import helper
import sync/co

div type stateP<a,e>
  Ready(
    id:int,
    p-ready: stateS<a,e> -> e list<(int, a)>
    )
  Blocked(
    id:int,
    p-blocked: (stateS<a,e>, a) -> e list<(int, a)>,
    waiting-on:int
    )

struct stateS<a,e> (
  q: funcqueue<stateP<a,e>> = Funcqueue(),
  done: list<(int, a)>      = [],
  pid: int                  =  1,
  pnext: int                =  2
)

fun runNext(st: stateS<a,<div|e>>): <div|e> list<(int,a)>
  match st.q.dequeue()
    Nothing -> st.done
    Just((Blocked(pid, res, waiting-on), q')) ->
      match st.done.lookup(fn(key) waiting-on==key)
        Nothing -> runNext(st(q=q'.enqueue(Blocked(pid, res, waiting-on))))
        Just(a) -> res(st(q=q', pid=pid), a)
    Just((Ready(pid, res), q')) -> res(st(q=q', pid=pid))

fun sched(state:stateS<a,<div|e>>, action:() -> <co<a>,div|e> a): <div|e> list<(int,a)>
  val res =
    with handler
      return(x)
        fn(st){
          val done' = Cons((st.pid, x), st.done)
          runNext(st(done=done'))
        }
      ctl uFork()
        fn(st){
          val resume' = fn(st') resume(0)(st')
          val pid = st.pnext
          val q' = st.q.enqueue(Ready(pid, resume'))
          resume(pid)(st(q=q', pnext=pid+1))
        }
      ctl wait(pid')
        fn(st){
          val p = match st.done.lookup(fn(key) pid'==key)
            Nothing -> Blocked(st.pid, fn(st', a) resume(a)(st'), pid' )
            Just(a) -> Ready(st.pid, fn(st') resume(a)(st))
          val q' = st.q.enqueue(p)
          runNext(st(q=q'))
        }
      ctl uInterrupt()
        fn(st){
          val resume' = fn(st') resume(())(st')
          val q' = st.q.enqueue(Ready(st.pid, resume'))
          runNext(st(q=q'))
        }
      ctl getPid()
        fn(st){
          resume(st.pid)(st)
        }
    action()
  res(state)


pub fun init(main: () -> <co<a>|e> ()): <co<a>|e> ()
  val pid = uFork()
  if pid == 0
  then
    main()
  else
    wait(pid)
    ()

pub fun timeshare2(action: () -> <co<a>,div|e> a): <div|e> list<(int,a)>
  with sched(StateS())
  action()

