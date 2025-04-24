module benches/full

import benches/bhelper

import fs
import basic
import basicIO
import sync
import pipe
import helper

fun crack_ceaser(x)
  fun crack_ceaser'(i:int, children:list<int>)
    val pid = uFork()
    if pid == 0 then
      ((fn() cat("image.enc")) || (fn() ceaser(i)) ||  (fn() head(2)) || paste) > (i.show ++ "-image.png")
      exit(0)
    else
      if i == 0 then children.iter(wait) else crack_ceaser'(i - 1, Cons(pid, children))
  crack_ceaser'(x, [])


fun full(n:int)

  switchUser(Alice)
  (fn() echo("FF FF FF FF FF FF 00 00 00\n FF FF FF FF FF FF 00 00 00\n FF FF FF FF FF FF 00 00 00")) > "image.png"
  ((fn() cat("image.png")) || (fn() ceaser(0-n)) || paste || rle) > "image.enc.rle"
  ((fn() cat("image.enc.rle")) || (fn() hash(379))) > "image.hash"
  // rm("image.png")

  switchUser(Bob)
  ((fn() cat("image.enc.rle")) || paste || unrle) > "image.enc"
  if !fork() then crack_ceaser(n)
  (fn() echo("FF FF FF AA AA AA 00 00 00")) >= (n.show ++ "-image.png")
  ((fn() cat(n.show ++ "-image.png")) || (fn() ceaser(0-n)) || paste || rle) > "image.enc.rle"

  switchUser(Alice)
  ((fn() cat("image.enc.rle")) || (fn() hash(379))) > "image-check.hash"
  (fn() echo("BOB!!"))

fun main()

  // val _ =
    val n = get-arg(10)
    with runState(FileSystem())
    with fileIO
    with sessionmgr2(Root)
    with timeshare2
    with retro-sync
    with interruptWrite
    with status
    full(n)
  // ()