module fs/file-io

import helper/state
import fs/storage
pub import fs/file-rw
pub import fs/file-co
pub import fs/file-lu
import fs/stream-redirection

import basic/exceptions

pub alias fileIO = <io/write, fs/file-rw/read, open, create, link, unlink>

pub fun fileIO(action: () -> <fileIO, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fileRW
  with fileCO
  with fileLU
  action()
  

pub fun cp(do-link: bool, src:string, dest:string): <io/write, fs/file-rw/read, open, create, link, exit> ()
  if do-link then link(src,dest)
  else match open(src)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
        Nothing -> exit(1)
        Just(cs) -> (fn() {echo(cs)}) > dest

pub fun rm(fname:string): unlink ()
  unlink(fname)