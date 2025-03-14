module sync/co

pub effect co<a>
  ctl uFork(): int
  ctl wait(pid: int): ()
  ctl uInterrupt(): ()
  ctl exitStatus(pid:int): maybe<a>
  ctl getPid():int


pub fun extractResult(pid:int): <co<a>, div> a
  match exitStatus(pid)
    Nothing ->
      wait(pid)
      extractResult(pid)
    Just(res) -> 
      res

pub effect cell<a>
  fun get(): a
  fun put(a:a): ()

pub fun runState(init:b, action: () -> <div, cell<b>|e> a): <div|e> a
  var curr := init
  with handler
    fun get()   curr
    fun put(x)  curr := x
  action() 

// pub alias queue<a> = list<(int,a)>

pub fun fib(n: int): <div, co<int>> int
  match n
    0 -> 1
    1 -> 1
    _ ->
      val pid1 = uFork()
      if pid1 == 0 then return fib(n - 1)
    
      val pid2 = uFork()
      if pid2 == 0 then return fib(n - 2)
 
      return extractResult(pid2) + extractResult(pid1)

