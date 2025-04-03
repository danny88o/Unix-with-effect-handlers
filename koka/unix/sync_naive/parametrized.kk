module sync_naive/parametrized

import basic
import helper
import sync_naive/co

// alias bproc<a,e> =
// alias proc<a,e> = stateS<a,e> -> <div|e> list<(int, a)>
// alias proc<a,e> = stateS<a,<div|e>> -> <div|e> list<(int, a)>

div type stateP<a,e>
  Ready(p-ready: stateS<a,e> -> e list<(int, a)>)
  Blocked(pid:int, p-blocked: (stateS<a,e>, a) -> e list<(int, a)>)

struct stateS<a,e> (
  q: list<(int, stateP<a,e>)> = [],
  done: list<(int, a)>        = [],
  pid: int                    =  1,
  pnext: int                  =  2
)

fun runNext(st: stateS<a,<div|e>>): <div|e> list<(int,a)>
  match st.q
    Nil -> st.done
    Cons((pid, Ready(res)), q') ->
      val st' = st(q=q', pid=pid)
      res(st')
    Cons((pid, Blocked(pid',res)), q') ->
      match st.done.lookup(fn(key) pid'==key)
        Nothing -> runNext(st(q=q'++[(pid, Blocked(pid',res))]))
        Just(a) -> res(st(q=q', pid=pid), a)

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
          val q' = st.q ++ [(pid, Ready(resume'))]
          resume(pid)(st(q=q', pnext=pid+1))
        }
      ctl wait(pid')
        fn(st){
          val p = match st.done.lookup(fn(key) pid'==key)
            Nothing -> Blocked(pid', fn(st', a) resume(a)(st'))
            Just(a) -> Ready(fn(st') resume(a)(st))
          val q' = st.q ++ [(st.pid, p)]
          runNext(st(q=q'))
        }
      ctl uInterrupt()
        fn(st){
          val resume' = fn(st') resume(())(st')
          val q' = st.q ++ [(st.pid, Ready(resume'))]
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

