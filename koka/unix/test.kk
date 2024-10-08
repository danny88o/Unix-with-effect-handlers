module test

import io
import exceptions
import time-sharing
import users

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


fun main()
  //println(test_io())
  //println(test_exception())
  println(test_user())