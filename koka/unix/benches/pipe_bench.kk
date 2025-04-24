module benches/pipe_bench

import benches/bhelper
import benches/hamlet
import unix

fun to-be(n:int)
  (fn() echo(hamlet)) > "scene.txt"
  val pipe = (fn() cat("scene.txt"))
    || (fn() head(n)) || paste
    || (fn() sed("be", "handle")) || (fn() sed("the", "a"))
    || freq
  pipe > "results.txt"
  rm("scene.txt")

fun main()
    val n = get-arg(20)
    with discard
    with unix
    to-be(n)
