module single_sched

import sync/co


alias proc = () -> <process, div> ()

div effect process
   ctl yield(): ()
   ctl fork(g: () -> <process, div, cell<list<proc>>> ()): ()

fun scheduler(f : () -> <process,cell<list<proc>>|e> ()) : <cell<list<proc>>|e> ()
  fun spawn(action : () -> <process, cell<list<proc>>|e> ()) : <cell<list<proc>>|e> ()
    with handler override
      ctl yield()
        val q = get() ++ [fn() resume(())]
        put(q)
      ctl fork(g)
        //val q = get() ++ [fn() resume(())]
        spawn(g)
    action()

  spawn(f)