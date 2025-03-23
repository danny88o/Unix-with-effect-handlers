module benches/full_better

import benches/helper
import benches/fs_unified

import fs/storage
import fs/unified
import basic
import basicIO
import sync/co
import sync/ref

fun full_mayhem(n: int)
  if n == 0 then return 0

  if whoami() == "root" then
    val pid = uFork()
    if pid == 0 then return fib(n)

    val calc = extractResult(pid)
    file_mayhem(calc)
    switchUser(Alice)
    return full_mayhem(n - 1)

  else
    switchUser(Root)
    return full_mayhem(n)

fun main()
  val _ =
    with fs/storage/runState(FileSystem())
    with fileIO
    with timeshare4
    with sessionmgr2(Root)
    with status
    full_mayhem(get-arg(16))
  ()