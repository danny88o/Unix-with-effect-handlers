module sync/ref

import basic
import helper
import sync/co

alias proc<a,e,h> = () -> <st<h>, div|e> ()

type stateP<a,e,h>
  Blocked(pid:int, res:proc<a,e,h>)
  Ready(res:proc<a,e,h>)

struct stateS<a,e,h> (
  q    :funcqueue<(int,stateP<a,e,h>)> = Funcqueue(),
  done :list<(int,a)>           = [],
  pid  :int                     =  1,
  pnext:int                     =  2
)


fun sched(init_state: stateS<a,e,h>, action: () -> <co<a>, st<h>, div|e> a):  <st<h>, div|e> list<(int, a)>
  val st = ref(init_state)

  fun runNext(state)
    match state.q.dequeue()
      Nothing -> st := state
      Just(((pid, Blocked(pid',res)), q')) ->
          if state.done.has(pid') then
            st := state(q=q', pid=pid)
            res()
          else runNext(state(q = q'.enqueue((pid, Blocked(pid',res)))))
      Just(((pid, Ready(res)), q')) ->
        st := state(q=q', pid=pid)
        res()

  fun run()
    with handler
      return(x)
        val done' = Cons(((!st).pid, x), (!st).done)
        runNext((!st)(done=done'))

      ctl uFork()
        val child_pid = (!st).pnext
        val child  = fn() resume(0)
        val q' = (!st).q.enqueue((child_pid, Ready(child)))
        st := (!st)(q=q', pnext=child_pid+1)
        resume(child_pid)

      ctl wait(pid')
        val resume' = fn() resume(())
        val q' = if (!st).done.has(pid')
          then (!st).q.enqueue(((!st).pid, Ready(resume')))
          else (!st).q.enqueue(((!st).pid, Blocked(pid', resume')))
        runNext((!st)(q=q'))

      ctl uInterrupt()
        val resume' = fn() resume(())
        val q' = (!st).q.enqueue(((!st).pid, Ready(resume')))
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
