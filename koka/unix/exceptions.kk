module exceptions

/* 
----------
Exceptions
----------
*/ 

effect ctl exit(code: int): ()

pub fun do_exit(c: int): exit ()
  exit(c)

pub fun status(action: () -> <exit|e> a ): e int
  with handler
    ctl exit(code) code
  action()
  0

  