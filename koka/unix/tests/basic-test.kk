module tests/basic-test

import io
import basic
import tests/helper

fun test_io()
  with basicIO
  echo("Hello")
  echo("World")
  5

fun test_exception()
  with basicIO
  with status
  echo("dead")
  exit(1)
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
  if fork() then 
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
  if fork() then 
    switchUser(Alice)
    echo(whoami() ++ "| ")
    star-wars()
  else
    switchUser(Bob)
    echo(whoami() ++ "| ")
    jar-jar()


fun main()
  println(test_io())
  println(test_exception())
  println(test_user())
  println(test_fork())
  println(test_interruption())
  
