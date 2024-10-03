module multiple_resumes

import std/num/random

effect ctl choice() : bool

fun xor() : choice bool
  val p = choice()
  val q = choice()
  if p then !q else q

fun choice-random(action : () -> <choice,random|e> a) : <random|e> a
  with fun choice() random-bool()
  action()

fun choice-all(action : () -> <choice|e> a) : e list<a>
  with handler
    return(x)    [x]
    ctl choice() resume(False) ++ resume(True)
  action()

fun main() 
  val rand:bool = choice-random(xor);
  println("Choice: " ++ rand.show) // ndet
  
  val all-rand = choice-all(xor);
  println("All Choices: " ++ all-rand.show)
