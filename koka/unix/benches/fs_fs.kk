module benches/fs_fs

import fs/storage
import benches/hamlet

import fs

import benches/helper

pub fun file_mayhem(n: int)
  match n
    0 -> ()
    _ ->
      val n' = n.show

      val name = "file" ++ n'
      create(name)
      val inum1 = unwrap(open(name))
      write(inum1, n')

      val middle = "mid"
      link(name, middle)

      val end = "end"
      create(end)

      val inum2 = unwrap(open(middle))
      val txt = unwrap(read(inum2))
      val inum3 = unwrap(open(end))
      write(inum3, txt)

      unlink(middle)
      unlink(end)

      file_mayhem(n - 1)


fun main()
  val n = get-arg(1000)
  val (fs, _) =
    with runState(FileSystem())
    with fileIO
    file_mayhem(n)
  println(fs)
