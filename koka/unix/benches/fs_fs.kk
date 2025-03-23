module benches/fs_fs

import fs/storage
import benches/hamlet

import fs

import benches/helper

pub fun file_mayhem(n: int)
  var i := 0
  while{i < n}
    val name = "file" ++ i.show
    println(create(name))
    val inum1 = match open(name) {Just(inum) -> inum}
    write(inum1, i.show)

    val middle = "mid"
    link(name, middle)
    val inum2 = match open(middle) {Just(inum) -> inum}
    val txt = match read(inum2) {Just(txt) -> txt}

    val other = "final_" ++ i.show
    create(other)
    val inum3 = match open(other) {Just(inum) -> inum}
    write(inum3, txt)

    unlink(middle)
    unlink(name)

    i := i + 1


fun main()
  val n = get-arg(1000)
  val (fs, _) =
    with runState(FileSystem())
    with fileIO
    file_mayhem(n)
  println(fs.dnext)
