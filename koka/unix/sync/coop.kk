module sync/coop

import sync/co
/*
Cooperative Multithreading
*/

// div type stateP<a,e>
//   Blocked(pid:int, res:proc<a,e>)
//   Ready(res:proc<a,e>)

// struct stateS<a,e> (
//   q    :list<(int,stateP<a,e>)> = [],
//   done :list<(int,a)>           = [],
//   pid  :int                     =  1,
//   pnext:int                     =  2
// )

alias container<a> = list<(int,a)>
alias eff = <coop, div, cell<container<coop>>>

pub div effect coop
  ctl yield(): ()
  fun spawn(f: () -> eff ()): ()


pub fun multithreading(action:() -> <eff|e> a): <div|e> ()
  var threads := []

  fun enqueue(t)
    threads := threads ++ [t]
  fun dequeue()
    match threads
      Nil -> ()
      Cons(t, ts) ->
        threads := ts
        t()

  with handler
    return(x)     dequeue()
    fun spawn(t)  enqueue(t)
    ctl yield()  {enqueue(fn() resume(())); dequeue()}

  action()

// pub fun fib(n: int): <coop|e> int


