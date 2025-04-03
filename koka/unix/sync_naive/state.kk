module sync_naive/state

import basic
import helper
import sync/co

alias proc<a,e> = () -> <cell<stateS<a,e>>, div|e> ()

div type stateP<a,e>
  Blocked(pid:int, res:proc<a,e>)
  Ready(res:proc<a,e>)

struct stateS<a,e> (
  q    :list<(int,stateP<a,e>)> = [],
  done :list<(int,a)>           = [],
  pid  :int                     =  1,
  pnext:int                     =  2
)

fun runNext(st:stateS<a,e>): <cell<stateS<a,e>>, div|e> ()
    match st.q
      Nil -> put(st)
      Cons((pid, Blocked(pid',res)), q') ->
          if st.done.has(pid') then
            put(st(q=q', pid=pid))
            res()
          else
            runNext(st(q = q'++[(pid, Blocked(pid',res))])) // Push to back of queue
      Cons((pid, Ready(res)), q') ->
        put(st(q=q', pid=pid))
        res()


fun sched(init_state: stateS<a,e>, action: () -> <co<a>, cell<stateS<a,e>>, div|e> a): <div|e> list<(int, a)>
  fun run(a)
    with handler
      return(x)
        val st = get()
        val done' = st.done ++ [(st.pid, x)]
        runNext(st(done=done'))

      ctl uFork()
        val st = get()
        val child_pid = st.pnext
        val child  = fn() resume(0)
        val q' = st.q ++ [(child_pid, Ready(child))]
        put(st(q=q', pnext=child_pid+1))
        resume(child_pid)

      ctl wait(pid')
        val st = get()
        val resume' = fn() resume(())
        val q' = if st.q.has(pid')
          then st.q ++ [(st.pid, Blocked(pid', resume'))]
          else st.q ++ [(st.pid, Ready(resume'))]
        runNext(st(q=q'))

      ctl uInterrupt()
        val st = get()
        val resume' = fn() resume(())
        val q' = st.q ++ [(st.pid, Ready(resume'))]
        runNext(st(q=q'))

      ctl exitStatus(pid)
        val st = get()
        val status = st.done.lookup(fn(key) pid==key)
        resume(status)

      ctl getPid()
        val st = get()
        resume(st.pid)
    a()

  with runState(init_state)
  run(action)
  get().done

pub fun timeshare3(action: () -> <co<a>, cell<stateS<a,e>>, div|e> a): <div|e> list<(int, a)>
  with sched(StateS())
  action()
