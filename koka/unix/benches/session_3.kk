module benches/session_3

import benches/bhelper
import basic
import unix

fun main()
  val n = get-arg(1000)
  with discard
  with unix_sm3
  chainSwitch(n)