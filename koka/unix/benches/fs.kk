module benches/fs

import fs/unified
import fs/storage
import benches/hamlet

// import fs/fs

import benches/helper

fun file_mayhem(n: int)
  var i := 0
  while{i < n}
    val name = "file" ++ i.show
    create(name)
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
  val n = 10//get-arg()
  val fs =
    with fileIO
    file_mayhem(n)
  println(fs)
