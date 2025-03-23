module benches/session_1

import benches/helper
import basic

fun main()
  val n = get-arg(1000)
  with sessionmgr(Root)
  chainSwitch(n)