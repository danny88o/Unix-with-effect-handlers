module stream-redirection

import io
import exceptions
import fs

pub infix 40 (>)

pub fun (>)(action: () -> <io/write|e> a, fname: string): <io/write,createFile,exit|e> a
  with fun io/write(fileDesc, fileContent)
    match createFile(fname) 
      Nothing   -> exit(1)
      Just(ino) -> write(ino, fileContent)
  with mask<exit>
  with mask<createFile>
  with mask<io/write>
  action()