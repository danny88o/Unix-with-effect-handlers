module unified

import helper
import basic
import fs/storage



/*
--------------------------
FileIO
--------------------------
*/
pub effect fileIO
  fun read(inum:int): maybe<string>
  fun write(inum:int, cs:string): ()
  fun create(fname: string): maybe<int>
  fun open(fname: string): maybe<int>
  fun link(src: string, dest: string): ()
  fun unlink(fname: string): ()


pub fun fileIO(action: () -> <cell<fileSystem>,fileIO|e> a): <cell<fileSystem>|e> a
  with handler
    fun read(ino)
      with withDefault(Nothing)
      val fs = get()
      Just(fs.fread(ino))
    fun write(ino, cs)
      with withDefault(())
      val fs = get()
      put(fs.fwrite(ino, cs))
    fun create(fname)
      with withDefault(Nothing)
      val (ino, fs) = get().fcreate(fname)
      put(fs)
      Just(ino)
    fun open(fname)
      with withDefault(Nothing)
      val fs = get()
      Just(fs.fopen(fname))
    fun link(src, dest)
      with withDefault(())
      val fs = get()
      put(fs.flink(src, dest))
    fun unlink(fname)
      with withDefault(())
      val fs = get()
      put(fs.funlink(fname))
  action()

/*
Stream Redirection
*/

pub fun (>)(action: () -> <fileIO, exit|e> a, fname: string): <fileIO,exit|e> a
  val maybe-ino = create(fname)
  with handler override
    fun write(fileDesc, fileContent)
      match maybe-ino
          Nothing   -> exit(1)
          Just(ino) -> write(ino, fileContent)
    fun read(ino)
      read(ino)
    fun create(fname_)
      create(fname_)
    fun open(fname_)
      open(fname_)
    fun link(src, dest)
      link(src, dest)
    fun unlink(fname_)
      unlink(fname_)

  action()


pub fun cp(do-link: bool, src:string, dest:string): <fileIO, exit> ()
  if do-link then link(src,dest)
  else match open(src)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
        Nothing -> exit(1)
        Just(cs) -> (fn() {write(1, cs)}) > dest

pub fun rm(fname:string): fileIO ()
  unlink(fname)

pub fun rename(from_name: string, to_name: string): <fileIO, exit> ()
  match open(from_name)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
      Nothing  -> exit(1)
      Just(cs) -> match create(to_name)
        Nothing -> exit(1)
        Just(ino2) -> write(ino2, cs)
  unlink(from_name)