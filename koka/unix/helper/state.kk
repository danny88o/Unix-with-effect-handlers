module helper/state

/*
--------------
Handling State
--------------
*/

pub effect cell<a>
  fun get(): a
  fun put(a:a): ()

pub fun runState(init:b, action: () -> <div, cell<b>|e> a): <div|e> (b,a)
  var curr := init
  with handler
    return(x)   (curr,x)
    fun get()   curr
    fun put(x)  curr := x
  action()  
