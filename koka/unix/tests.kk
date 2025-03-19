module tests

import basic
import basicIO
import fs
import pipe
import sync/co
import sync/parametrized

fun main()
  // println("\n\nBasic Tests:")
  // println(test_io())
  // println(test_exception())
  // println(test_user())
  // println(test_fork())
  // println(test_interruption())

  println("\n\nfs: ")
  println(test_stream-redirection())
  println("")
  println(test_fs())

  println("\n\nUtils: ")
  println(test_utils())

  println("\n\nSynchronisation")
  println(test_sync())


/*
Basic Functionality Tests
*/

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


/*
File System Tests
*/

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
    unix > "ritchie.txt"
  else
    switchUser(Bob)
    hamlet > "hamlet"

fun test_fs()
  with runState(FileSystem())
  with timeshare
  with fileIO
  with interruptWrite
  with sessionmgr(Root)
  with status

  if fork() then
    switchUser(Alice)
    unix > "ritchie.txt"
    cp(False,  "ritchie.txt", "ritchie")
    rm("ritchie.txt")
  else
    switchUser(Bob)
    hamlet > "hamlet"
    cp(True, "hamlet", "act3")


/*
Test Pipe
*/

fun test_utils()
  val p =
    (fn() cat("hamlet")) || (fn() head(2)) || paste ||
    (fn() sed("be,","be")) ||(fn() sed("To", "to")) || (fn() sed("question:", "question")) ||
    freq || renderTable

  with runState(FileSystem())
  with fileIO
  with timeshare
  with interruptWrite
  with sessionmgr(Root)
  with status
  hamlet > "hamlet"
  p > "analysis"

/*
Synchronisation
*/

fun test_sync()
  with runState(FileSystem())
  with fileIO
  with timeshare2
  with sessionmgr(Root)
  with status
  with init
  val pid = uFork()
  if pid == 0
  then {
    switchUser(Alice)
    unix()
  }
  else {
    switchUser(Bob)
    wait(pid)
    hamlet()
  }

/*
Helper functions for tests
*/

pub fun star-wars()
  echo("A long, long time ago ")
  echo("in a galaxy far, far away ")

pub fun jar-jar()
  echo("Mesa culled Jar Jar Binks ")
  echo("Mesa your humble servant ")

pub fun unix()
  echo("UNIX is basically ")
  echo("a simple operating system, ")
  echo("but ")
  echo("you have to be a genius to understand the simplicity.\n")

pub fun hamlet()
  echo("To be, or not to be, ")
  echo("that is the question:\n")
  echo("Whether 'tis nobler in the mind to suffer\n")
