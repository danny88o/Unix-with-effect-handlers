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



