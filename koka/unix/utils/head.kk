module utils/head

import utils/pipes

pub fun head(n:int): <await<char>, yield<char>, div> ()
  if n == 0 then yield('0')
  else 
    val c = await()
    yield(c)
    match c
      '0'  -> ()
      '\n' -> head(n - 1)
      _    -> head(n)