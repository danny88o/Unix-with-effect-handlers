module tests/fs-test

import basic
import fs
import tests/helper
import helper/state

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

fun main()
  println(test_stream-redirection())
  println(test_fs())

