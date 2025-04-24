module benches/session_2

import benches/bhelper
import basic
import unix

fun main()
  val n = get-arg(1000)
  with discard
  with unix_sm2
  chainSwitch(n)