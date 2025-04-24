module benches/sync_ref

import benches/bhelper

import unix

pub fun main()
  val n = get-arg()
  with discard
  with unix_ts4
  fib(n)
