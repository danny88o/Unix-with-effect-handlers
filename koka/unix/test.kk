module test

import io
import exceptions
import time-sharing
import users

import fs/fail
import fs/file-oc
import fs/file-rw
import fs/stream-redirection
import fs/state

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

fun star-wars()
  echo("A long, long time ago ")
  echo("in a galaxy far, far away ")

fun jar-jar()
  echo("Mesa culled Jar Jar Binks ")
  echo("Mesa your humble servant ")

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

fun test_stream-redirection()
  with runState(FileSystem())
  with fileRW
  with fileCO
  with timeshare
  with interruptWrite
  with sessionmgr(Root)
  with status
  if fork() then
    switchUser(Alice)
    star-wars > "title-creds.txt"
  else 
    switchUser(Bob)
    jar-jar > "sith"


fun main()
  println(test_io())
  println(test_exception())
  println(test_user())
  println(test_fork())
  println(test_interruption())
  val (fs, code) = test_stream-redirection()
  println("Exit Code: " ++ code.show)
  reveal(fs)
