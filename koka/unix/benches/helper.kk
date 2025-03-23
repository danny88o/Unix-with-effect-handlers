module benches/helper

import std/os/env

import sync/co

import basic

pub fun get-arg(def=5)
  get-args().head("").parse-int().default(def)




// sync
pub fun fib(n: int): <div, co<int>> int
  match n
    0 -> 1
    1 -> 1
    _ ->
      val pid1 = uFork()
      if pid1 == 0 then return fib(n - 1)

      val pid2 = uFork()
      if pid2 == 0 then return fib(n - 2)

      uInterrupt()

      return extractResult(pid2) + extractResult(pid1)

pub fun getPid1(action: () -> e list<(int,int)>): e int
  match action().lookup(fn(x) x == 1)
    Nothing -> -1
    Just(x) -> x

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

pub fun chainSwitch(n)
  with repeat(n)
  match whoami()
    "root" -> switchUser(Alice)
    "alice" -> switchUser(Bob)
    "bob" -> switchUser(Root)

