module sync/parametrized

import basic
import helper
import sync/co

//pub alias co = <uFork, wait, interrupt>
alias proc<a,e> = stateS<a,e> -> e list<(int, a)>

div type stateP<a,e>
  Blocked(pid:int, res:proc<a,e>)
  Ready(res: proc<a,e>)

struct stateS<a,e> (
  q: funcqueue<(int, stateP<a,e>)> = Funcqueue(),
  done: list<(int, a)>        = [],
  pid: int                    =  1,
  pnext: int                  =  2
)



fun runNext(st: stateS<a,<div|e>>): <div|e> list<(int,a)>
  match st.q.dequeue()
    Nothing -> st.done
    Just(((pid, Blocked(pid',res)), q')) ->
      if st.done.has(pid')
      then res(st(q=q', pid=pid))
      else runNext(st(q=q'.enqueue((pid, Blocked(pid',res))))) // Push to back of queue
    Just(((pid, Ready(res)), q')) -> res(st(q=q', pid=pid))

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
          val q' = st.q.enqueue((pid, Ready(resume')))
          resume(pid)(st(q=q', pnext=pid+1))
        }
      ctl wait(pid')
        fn(st){
          val resume' = fn(st') resume(())(st')
          val q' = if st.done.has(pid')
            then st.q.enqueue((st.pid, Ready(resume')))
            else st.q.enqueue((st.pid, Blocked(pid', resume')))
          runNext(st(q=q'))
        }
      ctl uInterrupt()
        fn(st){
          val resume' = fn(st') resume(())(st')
          val q' = st.q.enqueue((st.pid, Ready(resume')))
          runNext(st(q=q'))
        }
      ctl exitStatus(pid)
        fn(st){
          val status = st.done.lookup(fn(key) pid==key)
          resume(status)(st)
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

pub fun timeshare2(action: () -> <co<a>,div|e> a): <div|e> list<(int,a)>
  with sched(StateS())
  action()

