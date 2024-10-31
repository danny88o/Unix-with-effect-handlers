module fs/file-rw

import fs/state
import fs/fail

import io

/*
------------------------
File reading and writing
------------------------
*/

// write effect is defined in io
pub effect fun read(inum:int): maybe<string>

fun fread(ino:int, fs:fileSystem): fail string 
  fs.dreg.lookup-fail(fs.ilist.lookup-fail(ino).loc)

fun fwrite(ino:int, fs:fileSystem, content:string): fail fileSystem
  val loc = fs.ilist.lookup-fail(ino).loc
  val file = fs.dreg.lookup-fail(loc)
  val dreg = fs.dreg.modify(loc, file) 
  fs(dreg=dreg)

pub fun fileRW(action: () -> <read, io/write, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun read(inum:int)
    with withDefault(Nothing)
    Just(fread(inum,get()))
  with fun write(inum:int, content:string)
    with withDefault(())
    put(fwrite(inum, get(), content))

  action()
