module tests/utils-test

import helper/state
import fs
import utils
import basic

import tests/helper

fun p()
  (fn() cat("hamlet")) || (fn() head(2)) || paste ||
  //(fn() sed("be,","be")) ||(fn() sed("To", "to")) || (fn() sed("question:", "question")) ||
  freq || renderTable

fun test_utils()
  with runState(FileSystem())
  with fileIO
  with timeshare
  with interruptWrite
  with sessionmgr(Root)
  with status
  hamlet > "hamlet"
  p()()