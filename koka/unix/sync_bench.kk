module sync_bench

import synchronisation

fun fib(n: int): <div, co<int>, console> int
  match n
    0 -> 1
    1 -> 1
    _ ->
      val pid = uFork()

      if pid == 0 then return fib(n - 1) 
        // val fib1 = fib(n - 1) 
        // println("child")
        // println((n - 1, "->", fib1))
        // return fib1
      
      val fib2 = fib(n - 2)
      val fib1:int = extractResult(pid)
      val fib = fib1 + fib2  

      // println("parent "++ getPid().show ++ " with child " ++ pid.show)
      println((n - 2, "->", fib2))
      println((n - 1, "->", fib1))
      println("f(" ++ n.show ++ ") = " ++ fib.show)

      return fib 

fun main()
  with timeshare2
  with handler override 
    return(x:int)
      println("Process " ++ getPid().show ++ " has finished returning "++ x.show)
      x
    ctl uFork() 
      val pid = uFork()
      if pid == 0 then
        println("  I am child " ++ getPid().show)
      else
        println(getPid().show ++ " Fork! Child is " ++ pid.show)
      resume(pid)
    ctl wait(pid: int) 
      println("  Process " ++ getPid().show ++ " Waiting on " ++ pid.show)
      resume(wait(pid))
    ctl uInterrupt() resume(uInterrupt())
    ctl exitStatus(pid:int) 
      val res = exitStatus(pid)
      resume(res)
    ctl getPid() resume(getPid())

  fib(4)

