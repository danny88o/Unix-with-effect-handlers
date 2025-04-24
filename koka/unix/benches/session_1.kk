module benches/session_1

import benches/bhelper
import basic
import unix

fun main()
  val n = get-arg(1000)
  with discard
  with unix
  chainSwitch(n)