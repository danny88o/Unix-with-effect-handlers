module benches/bhelper

import std/os/env

import sync/co

import basic

pub fun get-arg(def=5)
  get-args().head("").parse-int().default(def)

pub fun discard(action)
  action()
  ()


// sync
pub fun fib(n: int): <div, co<int>> int
  match n
    0 -> 0
    1 -> 1
    _ ->
      val pid1 = uFork()
      if pid1 == 0 then return fib(n - 1)

      val pid2 = uFork()
      if pid2 == 0 then return fib(n - 2)

      uInterrupt()

      return wait(pid2) + wait(pid1)

pub fun getPid1(action: () -> e list<(int,int)>): e int
  match action().lookup(fn(x) x == 1)
    Nothing -> -1
    Just(x) -> x

pub fun sync_debug(action)
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
    fun getPid() getPid()
  action()

pub fun chainSwitch(n)
  if n == 0 then ()
  else
    match whoami()
      "root" -> switchUser(Alice)
      "alice" -> switchUser(Bob)
      "bob" -> switchUser(Root)
    chainSwitch(n - 1)

pub fun unwrap(x: maybe<a>): exn a
  match x
    Just(x) -> x