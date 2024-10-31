module fs/file-rw

import helper/fail
import helper/state
import fs/storage

pub import io

/*
------------------------
File reading and writing
------------------------
*/

// write effect is defined in io
pub effect fun read(inum:int): maybe<string>

fun fread(ino:int, fs:fileSystem): fail string 
  val inode = fs.ilist.lookup-fail(ino)
  fs.dreg.lookup-fail(inode.loc)

fun fwrite(ino:int, cs:string, fs:fileSystem): fail fileSystem
  val loc = fs.ilist.lookup-fail(ino).loc
  val file = fs.dreg.lookup-fail(loc)
  val dreg = fs.dreg.modify(loc, file++cs) 
  fs(dreg=dreg)

pub fun fileRW(action: () -> <read, io/write, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun read(ino)
    with withDefault(Nothing)
    Just(fread(ino,get()))
  with fun write(ino, cs)
    with withDefault(())
    put(fwrite(ino, cs, get()))

  action()
