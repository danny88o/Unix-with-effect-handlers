module sync/ref

import basic
import helper
pub import sync/co

alias proc<a,e,h> = () -> <st<h>, div|e> ()

type stateP<a,e,h>
  Blocked(pid:int, res:proc<a,e,h>)
  Ready(res:proc<a,e,h>)

struct stateS<a,e,h> (
  q    :list<(int,stateP<a,e,h>)> = [],
  done :list<(int,a)>           = [],
  pid  :int                     =  1,
  pnext:int                     =  2
)


fun sched(init_state: stateS<a,e,h>, action: () -> <co<a>, st<h>, div|e> a):  <st<h>, div|e> list<(int, a)>
  val st = ref(init_state)

  fun runNext(state)
    match state.q
      Nil -> st := state
      Cons((pid, Blocked(pid',res)), q') ->
          if state.done.has(pid') then
            st := state(q=q', pid=pid)
            res()
          else runNext(state(q = q'++[(pid, Blocked(pid',res))])) // Push to back of queue
      Cons((pid, Ready(res)), q') ->
        st := state(q=q', pid=pid)
        res()

  fun run()
    with handler
      return(x)
        val done' = (!st).done ++ [((!st).pid, x)]
        runNext((!st)(done=done'))

      ctl uFork()
        val child_pid = (!st).pnext
        val child  = fn() resume(0)
        val q' = (!st).q ++ [(child_pid, Ready(child))]
        st := (!st)(q=q', pnext=child_pid+1)
        resume(child_pid)

      ctl wait(pid')
        val resume' = fn() resume(())
        val q' = if (!st).q.has(pid')
          then (!st).q ++ [((!st).pid, Blocked(pid', resume'))]
          else (!st).q ++ [((!st).pid, Ready(resume'))]
        runNext((!st)(q=q'))

      ctl uInterrupt()
        val resume' = fn() resume(())
        val q' = (!st).q ++ [((!st).pid, Ready(resume'))]
        runNext((!st)(q=q'))

      ctl exitStatus(pid)
        val status = (!st).done.lookup(fn(key) pid==key)
        resume(status)

      ctl getPid()
        resume((!st).pid)

    action()

  run()
  (!st).done

pub fun timeshare4(action)
  with sched(StateS())
  action()
