module exceptions

/* 
----------
Exceptions
----------
*/ 

pub effect ctl exit(code: int): ()

pub fun status(action: () -> <exit|e> a ): e int
  with ctl exit(code) code
  action()
  0

  