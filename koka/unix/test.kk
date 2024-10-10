module test

import io
import exceptions
import time-sharing
import users
import time-sharing

fun test_io()
  with basicIO
  echo("Hello")
  echo("World")
  5

fun test_exception()
  with basicIO
  with status
  echo("dead")
  do_exit(1)
  echo("code")

fun test_user()
  with basicIO
  with sessionmgr(Root)
  with status
  echo(whoami() ++ " ")
  switchUser(Alice)
  echo(whoami() ++ " ")

fun test_fork()
  with basicIO
  with nondet
  with sessionmgr(Root)
  with status
  if do_fork() then 
    switchUser(Alice)
    echo(whoami() ++ " ")
  else
    switchUser(Bob)
    echo(whoami() ++ " ")

fun test_interruption()
  with basicIO
  with timeshare
  with interruptWrite
  with sessionmgr(Root)
  with status
  if do_fork() then 
    switchUser(Alice)
    echo(whoami() ++ "| ")
    echo("A long, long time ago ")
    echo("in a galaxy far, far away ")
  else
    switchUser(Bob)
    echo(whoami() ++ "| ")
    echo("Mesa culled Jar Jar Binks ")
    echo("Mesa your humble servant ")

fun main()
  println(test_io())
  println(test_exception())
  println(test_user())
  println(test_fork())
  println(test_interruption())