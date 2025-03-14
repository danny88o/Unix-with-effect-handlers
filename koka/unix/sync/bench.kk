module sync/bench

import helper
import sync/co
import sync/parametrized
import sync/synchope

val n = 16

fun getPid1(action: () -> e list<(int,int)>): e int    
  match action().lookup(fn(x) x == 1)
    Nothing -> -1
    Just(x) -> x

fun param_fib()
  with timeshare2
  fib(n)

fun state_fib()
  with timeshare3
  fib(16)

fun main()
  with getPid1
  with timeshare2
  // with timeshare3
  fib(n)


  // with handler override 
  //   return(x:int)
  //     println("Process " ++ getPid().show ++ " has finished returning "++ x.show)
  //     x
  //   ctl uFork() 
  //     val pid = uFork()
  //     if pid == 0 then
  //       println("  I am child " ++ getPid().show)
  //     else
  //       println(getPid().show ++ " Fork! Child is " ++ pid.show)
  //     resume(pid)
  //   ctl wait(pid: int) 
  //     println("  Process " ++ getPid().show ++ " Waiting on " ++ pid.show)
  //     resume(wait(pid))
  //   ctl uInterrupt() resume(uInterrupt())
  //   ctl exitStatus(pid:int) 
  //     val res = exitStatus(pid)
  //     resume(res)
  //   ctl getPid() resume(getPid())
