module sync/ref

import basic
import helper
import sync/co

alias proc<e,h> = () -> <st<h>, div|e> ()
alias bproc<a,e,h> = a -> <st<h>, div|e> ()

div type stateP<a,e,h>
  Ready(res:proc<e,h>)
  Blocked(pid:int, bres:bproc<a,e,h>)

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
          match state.done.lookup(fn(key) pid==key)
            Nothing -> runNext(state(q = q'.enqueue((pid, Blocked(pid',res)))))
            Just(a) ->
              st := state(q=q', pid=pid)
              res(a)
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
        val p = match (!st).done.lookup(fn(key) pid'==key)
          Nothing -> Blocked(pid', fn(a) resume(a))
          Just(a) -> Ready(fn() resume(a))
        val q' = (!st).q.enqueue(((!st).pid, p))
        st := (!st)(q=q')

      ctl uInterrupt()
        val resume' = fn() resume(())
        val q' = (!st).q.enqueue(((!st).pid, Ready(resume')))
        runNext((!st)(q=q'))

      fun getPid()
        (!st).pid

    action()

  run()
  (!st).done

pub fun timeshare4(action)
  with sched(StateS())
  action()
