module benches/fs_unified

import fs/storage
import benches/hamlet

import basic
import fs/unified
import helper

import benches/bhelper

pub fun file_mayhem(n: int)
  match n
    0 -> ()
    _ ->
      val n' = n.show
      (fn() echo(n')) >= "countdown"
      // (fn() echo("\n")) >= "countdown"

      (fn() echo(n')) > "temp"
      cp(True, "temp", "mid")
      cp(False, "mid", n')

      rm("temp")
      rm("mid")

      file_mayhem(n - 1)


fun main()
  val n = get-arg(2)
  val (_, _) =
    with runState(FileSystem())
    with fileIO
    with status
    (fn() echo("")) > "countdown"
    file_mayhem(n)
  // println(fs)
  ()
