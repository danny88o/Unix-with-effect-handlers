module fs

pub import basicIO
import basic
import helper
import fs/storage


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
    val fs = get()
    Just(fs.fread(ino))
  with fun write(ino, cs)
    with withDefault(())
    val fs = get()
    put(fs.fwrite(ino, cs))

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
    val (ino, fs) = get().fcreate(fname)
    put(fs)
    Just(ino)
  with fun open(fname)
    with withDefault(Nothing)
    val fs = get()
    Just(fs.fopen(fname))
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
      val fs = get()
      put(fs.flink(src, dest))
  with fun unlink(fname)
      with withDefault(())
      val fs = get()
      put(fs.funlink(fname))
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
  with fileLU
  with fileRW
  with fileCO
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

// pub fun compactFileIO(action: () -> <fileIO, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
//   with fun read(ino)
//       with withDefault(Nothing)
//       val fs = get()
//       Just(fs.fread(ino))
//   with fun write(ino, cs)
//       with withDefault(())
//       val fs = get()
//       put(fs.fwrite(ino, cs))
//   with fun create(fname)
//       with withDefault(Nothing)
//       val (ino, fs) = get().fcreate(fname)
//       put(fs)
//       Just(ino)
//   with fun open(fname)
//       with withDefault(Nothing)
//       val fs = get()
//       Just(fs.fopen(fname))
//   with fun link(src, dest)
//         with withDefault(())
//         val fs = get()
//         put(fs.flink(src, dest))
//   with fun unlink(fname)
//         with withDefault(())
//         val fs = get()
//         put(fs.funlink(fname))
//   action()