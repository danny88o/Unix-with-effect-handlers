module coop


/*
Cooperative Multithreading
*/
pub effect coop
  ctl yield(): ()
  fun spawn(f: () -> ()): ()

pub fun multithreading(action:() -> <coop,div|e> a): <div|e> ()
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
  

  