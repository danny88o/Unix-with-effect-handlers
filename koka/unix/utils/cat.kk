module utils/cat

import utils/pipes

import helper/list

import basic/exceptions

import fs/file-io

pub fun cat(fname: string): <fileIO, yield<char>, exit> ()
  match open(fname)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
      Nothing  -> exit(1)
      Just(cs) -> 
        cs.iter(yield)
        yield('0')

