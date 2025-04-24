module sync/state

import basic
import helper
import sync/co

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

div type proc<a,e>
  Ready(id:int, p-ready: () -> <cell<stateS<a,e>>, div|e> list<(int, a)>)
  Blocked(id:int, p-blocked: a -> <cell<stateS<a,e>>, div|e> list<(int, a)>, waiting-on:int)

struct stateS<a,e>(
  q    :funcqueue<proc<a,e>> = Funcqueue(),
  done :list<(int,a)>        = [],
  pid  :int                  =  1,
  pnext:int                  =  2
)

fun runNext(st:stateS<a,e>): <cell<stateS<a,e>>, div|e> list<(int, a)>
    match st.q.dequeue()
      Nothing -> st.done
      Just((Blocked(pid, res, waiting-on), q')) ->
          match st.done.lookup(fn(key) waiting-on==key)
            Nothing ->
              runNext(st(q=q'.enqueue(Blocked(pid, res, waiting-on))))
            Just(a) ->
              put(st(q=q', pid=pid))
              res(a)
      Just((Ready(pid, res), q')) ->
        put(st(q=q', pid=pid))
        res()

fun sched(action: () -> <co<a>, cell<stateS<a,e>>, div|e> a): <cell<stateS<a,e>>, div|e> list<(int, a)>
  with handler
    return(x)
      val st = get()
      val done' = Cons((st.pid, x), st.done)
      runNext(st(done=done'))

    ctl uFork()
      val st = get()
      val child_pid = st.pnext
      val p = Ready(child_pid, fn() resume(0))
      val q' = st.q.enqueue(p)
      put(st(q=q', pnext=child_pid+1))
      resume(child_pid)

    ctl wait(pid')
      val st = get()
      val p = Blocked(st.pid, fn(a) resume(a), pid')
      val q' = st.q.enqueue(p)
      runNext(st(q=q'))

    ctl uInterrupt()
      val st = get()
      val p = Ready(st.pid, fn() resume(()))
      val q' = st.q.enqueue(p)
      runNext(st(q=q'))

    fun getPid()
      val st = get()
      st.pid
  action()

pub fun timeshare3(action: () -> <co<a>, cell<stateS<a,e>>, div|e> a): <div|e> list<(int, a)>
  val (_, done) =
    with runState(StateS())
    with sched
    action()
  done
