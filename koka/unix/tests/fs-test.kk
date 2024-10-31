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
    star-wars > "title-creds.txt"
  else 
    switchUser(Bob)
    jar-jar > "sith"

fun test_fs()
  with runState(FileSystem())
  with timeshare
  with fileIO
  with interruptWrite
  with sessionmgr(Root)
  with status
  if fork() then
    switchUser(Alice)
    star-wars > "title-creds.txt"
    cp(False,  "title-creds.txt", "ritchie")
    rm("ritchie.txt")
  else 
    switchUser(Bob)
    jar-jar > "sith"
    cp(True, "sith", "act3")

fun main()
  println(test_stream-redirection())
  println(test_fs())

