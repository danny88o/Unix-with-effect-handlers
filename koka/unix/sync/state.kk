module sync/state

import basic
import helper
import sync/co

pub effect cell<a>
  fun get(): a
  fun put(a:a): ()

pub fun runState(init:b, action: () -> <div, cell<b>|e> a): <div|e> a
  var curr := init
  with handler
    fun get()   curr
    fun put(x)  curr := x
  action()

div type proc<a,e>
  Ready(id:int, p-ready: () -> <cell<stateS<a,e>>, div|e> ())
  Blocked(id:int, p-blocked:a -> <cell<stateS<a,e>>, div|e> (), waiting-on:int)

struct stateS<a,e>(
  q    :funcqueue<proc<a,e>> = Funcqueue(),
  done :list<(int,a)>        = [],
  pid  :int                  =  1,
  pnext:int                  =  2
)

fun runNext(st:stateS<a,e>): <cell<stateS<a,e>>, div|e> ()
    match st.q.dequeue()
      Nothing -> put(st)
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


fun sched(init_state: stateS<a,e>, action: () -> <co<a>, cell<stateS<a,e>>, div|e> a): <div|e> list<(int, a)>
  fun run(m)
    with handler
      return(x)
        val st = get()
        val done' = Cons((st.pid, x), st.done)
        runNext(st(done=done'))

      ctl uFork()
        val st = get()
        val child_pid = st.pnext
        val child  = fn() resume(0)
        val q' = st.q.enqueue(Ready(child_pid, child))
        put(st(q=q', pnext=child_pid+1))
        resume(child_pid)

      ctl wait(pid')
        val st = get()
        val p = match st.done.lookup(fn(key) pid'==key)
          Nothing -> Blocked(pid', fn(a) resume(a), st.pid)
          Just(a) -> Ready(st.pid, fn() resume(a))
        val q' = st.q.enqueue(p)
        runNext(st(q=q'))

      ctl uInterrupt()
        val st = get()
        val resume' = fn() resume(())
        val q' = st.q.enqueue(Ready(st.pid, resume'))
        runNext(st(q=q'))
      fun getPid()
        val st = get()
        st.pid
    m()

  with runState(init_state)
  run(action)
  get().done

pub fun timeshare3(action: () -> <co<a>, cell<stateS<a,e>>, div|e> a): <div|e> list<(int, a)>
  with sched(StateS())
  action()
