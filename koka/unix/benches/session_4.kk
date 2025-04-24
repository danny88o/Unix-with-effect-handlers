module benches/session_4

import basic
import benches/bhelper
import unix

pub fun chainSwitch2(n)
  if n == 0 then ()
  else
    match ask2
      "root" -> switchUser(Alice)
      "alice" -> switchUser(Bob)
      "bob" -> switchUser(Root)
    chainSwitch2(n - 1)

fun main()
  val n = get-arg(5)
  with discard
  with unix_sm4
  chainSwitch2(n)