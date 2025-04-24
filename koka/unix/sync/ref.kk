module sync/ref

import basic
import helper
import sync/co

alias proc<a,e> = () -> <div|e> list<(int, a)>
alias bproc<a,e> = a -> <div|e> list<(int, a)>

pub type funcqueue<a>
  Funcqueue(
    enter: list<a> = [],
    out: list<a> = []
  )

pub fun enqueue<a>(q:funcqueue<a>, x:a): funcqueue<a>
  match q
    Funcqueue(in_q, out_q) -> Funcqueue(Cons(x,in_q), out_q)


pub fun dequeue<a>(q:funcqueue<a>): div maybe<(a, funcqueue<a>)>
  match q
    Funcqueue(Nil, Nil) -> Nothing
    Funcqueue(in_q, Nil) -> dequeue(Funcqueue(Nil, in_q.reverse()))
    Funcqueue(in_q, Cons(x, xs)) -> Just((x, Funcqueue(in_q, xs)))

type stateP<a,e>
  Ready(pid: int, res:proc<a,e>)
  Blocked(pid:int, bres:bproc<a,e>, waiting-on:int)

struct stateS<a,e> (
  q    :funcqueue<(stateP<a,e>)> = Funcqueue(),
  done :list<(int,a)>           = [],
  pid  :int                     =  1,
  pnext:int                     =  2
)


fun sched(init_state: stateS<a,<st<h>|e>>, action: () -> <co<a>, st<h>, div|e> a): <st<h>, div|e> list<(int, a)>
  val state = ref(init_state)
  fun runNext(st)
    match st.q.dequeue()
      Nothing -> st.done
      Just((Blocked(pid, res, waiting-on), q')) ->
          match st.done.lookup(fn(key) waiting-on==key)
            Nothing -> runNext(st(q = q'.enqueue((Blocked(pid, res, waiting-on)))))
            Just(a) ->
              state := st(q=q', pid=pid)
              res(a)
      Just((Ready(pid, res), q')) ->
        state := st(q=q', pid=pid)
        res()

  with handler
    return(x)
      val st = !state
      val done' = Cons((st.pid, x), st.done)
      runNext(st(done=done'))

    ctl uFork()
      val st = !state
      val child_pid = st.pnext
      val p = Ready(child_pid, fn() resume(0))
      val q' = st.q.enqueue(p)
      state := st(q=q', pnext=child_pid+1)
      resume(child_pid)

    ctl wait(pid')
      val st = !state
      val p = Blocked(st.pid, fn(a) resume(a), pid')
      val q' = st.q.enqueue(p)
      runNext(st(q=q'))

    ctl uInterrupt()
      val st = !state
      val p = Ready(st.pid, fn() resume(()))
      val q' = st.q.enqueue(p)
      runNext(st(q=q'))

    fun getPid()
      (!state).pid

  action()


pub fun timeshare4(action)
  with sched(StateS())
  action()
