module fs

import basicIO
import basic
import helper
import fs/storage


/*
--------------
Handling State
--------------
*/

pub effect cell<a>
  fun get(): a
  fun put(a:a): ()

pub fun runState(init:b, action: () -> <div, cell<b>|e> a): <div|e> (b,a)
  var curr := init
  with handler
    return(x)   (curr,x)
    fun get()   curr
    fun put(x)  curr := x
  action()

/*
------------------------
File reading and writing
------------------------
*/

// write effect is defined in basicIO/write
pub effect fun read(inum:int): maybe<string>
pub alias fileRW = <basicIO/write, read>


pub fun fileRW(action: () -> <fileRW, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun read(ino)
    with withDefault(Nothing)
    Just(fread(ino, get()))
  with fun write(ino, cs)
    with withDefault(())
    put(fwrite(ino, cs, get()))

  action()


/*
--------------------------
File creating and opening
--------------------------
*/

pub effect fun create(fname: string): maybe<int>
pub effect fun open(fname: string): maybe<int>
pub alias fileCO = <create, open>

pub fun fileCO(action: () -> <fileCO, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun create(fname)
    with withDefault(Nothing)
    val (ino, fs) = fcreate(fname, get())
    put(fs)
    Just(ino)
  with fun open(fname)
    with withDefault(Nothing)
    Just(fopen(fname, get()))
  action()


/*
--------------------------
File linking and unlinking
--------------------------
*/

pub effect fun link(src: string, dest: string): ()
pub effect fun unlink(fname: string): ()
pub alias fileLU = <link, unlink>


pub fun fileLU(action: () -> <fileLU, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun link(src, dest)
      with withDefault(())
      val fs = flink(src, dest, get())
      put(fs)
  with fun unlink(fname)
      with withDefault(())
      val fs = funlink(fname, get())
      put(fs)
  action()


/*
Stream Redirection
*/

pub fun (>)(action: () -> <basicIO/write, create, exit|e> a, fname: string): <basicIO/write,create,exit|e> a
  val maybe-ino = create(fname)

  with override fun write(fileDesc, fileContent)
    match maybe-ino
        Nothing   -> exit(1)
        Just(ino) -> basicIO/write(ino, fileContent)

  action()


/*
--------------------------
FileIO
--------------------------
*/

pub alias fileIO = <basicIO/write, read, open, create, link, unlink>

pub fun fileIO(action: () -> <fileIO, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fileRW
  with fileCO
  with fileLU
  action()


pub fun cp(do-link: bool, src:string, dest:string): <basicIO/write, read, open, create, link, exit> ()
  if do-link then link(src,dest)
  else match open(src)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
        Nothing -> exit(1)
        Just(cs) -> (fn() {echo(cs)}) > dest

pub fun rm(fname:string): unlink ()
  unlink(fname)

pub fun compactFileIO(action: () -> <fileIO, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun read(ino)
      with withDefault(Nothing)
      Just(fread(ino,get()))
  with fun write(ino, cs)
      with withDefault(())
      put(fwrite(ino, cs, get()))
  with fun create(fname)
      with withDefault(Nothing)
      val (ino, fs) = fcreate(fname, get())
      put(fs)
      Just(ino)
  with fun open(fname)
      with withDefault(Nothing)
      Just(fopen(fname, get()))
  with fun link(src, dest)
        with withDefault(())
        val fs = flink(src, dest, get())
        put(fs)
  with fun unlink(fname)
        with withDefault(())
        val fs = funlink(fname, get())
        put(fs)
  action()