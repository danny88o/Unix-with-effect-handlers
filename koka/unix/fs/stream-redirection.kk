module fs/stream-redirection

import io
import basic/exceptions
import fs/file-co

pub fun (>)(action: () -> <io/write, create, exit|e> a, fname: string): <io/write,create,exit|e> a
  with override fun write(fileDesc, fileContent)
    match create(fname) 
      Nothing   -> exit(1)
      Just(ino) -> io/write(ino, fileContent)
  action()
