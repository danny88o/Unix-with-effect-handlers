module exceptions

/* 
----------
Exceptions
----------
*/ 

effect status
  ctl exit(code: int): ()

pub fun do_exit(c: int): status ()
  exit(c)

pub fun status(action: () -> <status|e> a ): e int
  with handler
    ctl exit(code) code
  action()
  0

  