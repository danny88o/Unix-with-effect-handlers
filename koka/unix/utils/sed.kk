module utils/sed

import utils/pipes 

pub fun sed(target: string, str':string): <await<string>, yield<string>, div> ()
  val str = await()
  if str == target then yield(str') else yield(str)
  sed(target, str')
