module benches/sync_param

import benches/bhelper
import unix

fun main()
  val n = get-arg()
  with discard
  with unix
  fib(n)


