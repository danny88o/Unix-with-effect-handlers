module benches/fs_fs


import benches/bhelper
import unix

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
  with discard
  with runState(FileSystem())
  with fileIO
  with status
  (fn() echo("")) > "countdown"
  file_mayhem(n)

