module benches/full

import benches/helper

import fs
import basic
import basicIO
import sync

fun memoize(file, result: int)
  switchUser(Root)
  create(file)
  (fn() echo(result.show)) > file
  switchUser(Alice)

pub fun fib_memo(n: int)
  val file: string = n.show
  match open(file)
    Nothing ->
      match n
        0 ->
          memoize(file, 0)
          exit(0)

        1 ->
          memoize(file, 1)
          exit(1)
        _ ->
          val pid1 = uFork()
          if pid1 == 0 then fib_memo(n - 1)
          else
            val pid2 = uFork()
            if pid2 == 0 then fib_memo(n - 2)
            else
              uInterrupt()
              val result:int = extractResult(pid2) + extractResult(pid1)
              memoize(file, result)
              exit(result)

    Just(ino) ->
      exit(read(ino).unwrap().parse-int().unwrap())


fun main()
  val _ =
    with fs/storage/runState(FileSystem())
    with fileIO
    with sessionmgr2(Root)
    with timeshare2
    with status
    val n = get-arg(2000)
    var i := 0
    while{ i < n }
      val pid = uFork()
      if pid == 0
      then fib_memo(i)
      else wait(pid)
      i := i + 1
  ()