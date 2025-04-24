module sync/coop

import sync/co
import helper
/*
Cooperative Multithreading
*/

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


div type stateP<a,e>
  Blocked(id:int, res-b:a -> <cell<stateS<a,e>>, div|e> (), waiting-on:int)
  Ready(id:int, res-r:() -> <cell<stateS<a,e>>, div|e> ())

struct stateS<a,e> (
  q    :funcqueue<stateP<a,e>>  = Funcqueue(),
  done :list<(int,a)>           = [],
  pid  :int                     =  1,
  pnext:int                     =  2
)

alias container<a> = list<(int,a)>

pub div effect coop<a,e>
  fun spawn(f: () -> <coop<a,e>, cell<stateS<a,e>>, div|e> a): int
  ctl yield(): ()
  ctl await(pid:int): a
  fun pid(): int

fun runNext(st)
  match st.q.dequeue()
    Nothing -> ()
    Just((Blocked(pid, res, waiting-on), q')) ->
        match st.done.lookup(fn(key) waiting-on==key)
          Nothing -> runNext(st(q = q'.enqueue((Blocked(pid, res, waiting-on)))))
          Just(a) ->
            put(st(q=q', pid=pid))
            res(a)
    Just((Ready(pid, res), q')) ->
      put(st(q=q', pid=pid))
      res()

pub fun sched(
  rhandler:(() -> <coop<a,e>, cell<stateS<a,e>>, div|e> a) -> <coop<a,e>, cell<stateS<a,e>>, div> a,
  action:() -> <coop<a,e>, cell<stateS<a,e>>, div|e> a
  ): <cell<stateS<a,e>>, div|e> ()
  fun sched'(a)
    with handler
      return(x)
        val st = get()
        val done' = Cons((st.pid, x), st.done)
        runNext(st(done=done'))

      fun spawn(f)
        val st = get()
        val child_pid = st.pnext

        val p = Ready(child_pid, fn() sched'(f))
        val q' = st.q.enqueue(p)
        put(st(q=q', pnext=child_pid+1))
        child_pid

      ctl yield()
        val st = get()
        val p = Ready(st.pid, fn() resume(()))
        val q' = st.q.enqueue(p)
        runNext(st(q=q'))

      ctl await(pid')
        val st = get()
        val p = Blocked(st.pid, fn(x) resume(x), pid')
        val q' = st.q.enqueue(p)
        runNext(st(q=q'))

      fun pid()
        val st = get()
        st.pid
    with rhandler
    a()
  with sched'
  action()


fun timeshare5(rhandler, action)
  with runState(StateS())
  with sched(rhandler)
  action


