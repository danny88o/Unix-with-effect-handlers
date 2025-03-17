module benches/sync_param

import benches/helper

import sync/parametrized

fun main()
  val n = get-arg()
  with getPid1
  with timeshare2
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
