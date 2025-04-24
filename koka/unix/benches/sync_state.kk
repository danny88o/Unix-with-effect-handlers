module benches/sync_state

import benches/bhelper

import unix

fun main()
  val n = get-arg()
  with discard
  with unix_ts3
  fib(n)
