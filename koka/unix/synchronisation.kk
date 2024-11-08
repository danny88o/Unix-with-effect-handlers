module synchronisation

import basic
import helper

pub effect co 
  ctl uFork(): int
  ctl wait(pid: int): ()
  ctl uInterrupt(): ()
//pub alias co = <uFork, wait, interrupt>

abstract type ix

alias proc<a> = stateS<a> -> list<(int,a)>

rec type stateP<a>
  Blocked(pid:int, res:proc<a>)
  Ready(res: proc<a>)

struct stateS<a>(q:list<(int,stateP<a>)>, done:list<(int,a)>, pid:int, pnext:int)

fun runNext(st: stateS<a>): div list<(int,a)>
  match st.q
    Nil -> st.done
    Cons((pid, p), q') -> match p
      Blocked  -> runNext(st(q = q' ++ [(pid, p)])) // Push to back of queue
      Ready(res) -> res(st(q = q', pid=pid))


fun sched(st:stateS<a>, action:() -> <co|e> a): <div|e> list<(int,a)>
  with handler
    return(x) 
      val done' = [(st.pid, x)] ++ st.done
      runNext(st(done=done'))
    raw ctl uFork()
      val resume' = fn(st') rcontext.resume(0)
      val pid = st.pnext
      val q' = st.q ++ [(pid, Ready(resume'))]
      with sched(st(q=q', pnext = pid+1))
      rcontext.resume(pid)
    raw ctl wait(pid)
      val resume' = fn(st') rcontext.resume(())
      val q' = if st.q.has(pid) then st.q ++ [(st.pid, Blocked(pid, resume'))] else st.q ++ [(st.pid, Ready(resume'))]
      runNext(st(q=q'))
    raw ctl uInterrupt()
      val resume' = fn(st') rcontext.resume(st')
      val q' = st.q ++ [(st.pid, Ready(resume'))]
      runNext(st(q=q'))
  action()




// val state-param = handler(s) 
//   return x -> (x,s)
//   get()    -> resume(s,s)
//   put(s')  -> resume((),s')

// fun toProc(res): proc<a>
//   fn(st:stateS<a>) 

