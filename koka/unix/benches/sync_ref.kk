module benches/sync_ref

import benches/helper

import sync/ref

pub fun main()
  val n = get-arg()
  with getPid1
  with timeshare4
  fib(n)
