module synchronisation

import basic

pub effect val uFork: int
pub effect fun wait(pid: int): ()
pub alias co = <uFork, wait, interrupt>

alias proc<a> = stateS<a> -> list<a>

rec type stateP<a>
  Blocked(pid:int, proc:proc<a>)
  Ready(proc: proc<a>)

struct stateS<a>(q:list<(int,stateP<a>)>, done:list<a>, pid:int, pnext:int)

fun runNext(st: stateS<a>): div list<a>
  match st.q
    Nil -> st.done
    Cons((pid, p), q') -> match p
      Blocked  -> runNext(st(q = q' ++ [(pid, p)])) // Push to back of queue
      Ready(proc) -> proc(st(q = q', pid=pid))

fun scheduler(action:() -> <co|e> a, st0:stateS<a>): list<(int,a)>
  var st := st0
  with 
  
  action()
