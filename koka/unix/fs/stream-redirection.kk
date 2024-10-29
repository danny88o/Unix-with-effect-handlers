module stream-redirection

import io
import exceptions
import fs

pub fun (>)(action: () -> <io/write|e> a, fname: string): <io/write,create,exit|e> a
  with fun io/write(fileDesc, fileContent)
    match create(fname) 
      Nothing   -> exit(1)
      Just(ino) -> io/write(ino, fileContent)
  with mask<exit>
  with mask<create>
  with mask<io/write>
  action()
