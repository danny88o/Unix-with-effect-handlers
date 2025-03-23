module benches/session_2

import benches/helper
import basic

fun main()
  val n = get-arg(1000)
  with sessionmgr2(Root)
  chainSwitch(n)