module state

effect state
  fun get(): int
  fun put(a:int): ()

fun runState(init: int, action: () -> state a): a
  var curr := init
  with handler
    fun get()   curr
    fun put(x)  curr := x
  action()

fun incOne(): state ()
  put(get()+1)

fun main()
  with runState(41)
  incOne()  
  get()


