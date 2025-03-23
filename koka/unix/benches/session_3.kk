module benches/session_3

import benches/helper
import basic

fun main()
  val n = get-arg(1000)
  with sessionmgr3(Root)
  chainSwitch(n)