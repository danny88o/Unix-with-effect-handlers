module benches/full_standard

import benches/helper
import benches/fs_fs

import fs
import basic
import basicIO
import sync/co
import sync/parametrized

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
    with timeshare2
    with sessionmgr(Root)
    with status
    full_mayhem(get-arg(16))
  ()


