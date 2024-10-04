module exceptions

/* 
----------
Exceptions
----------
*/ 

effect status
  ctl exit(code: int): a


pub fun status(action: () -> <status|e> a ): e int
  with ctl exit(code) code
  action()
  0
