module unified

import helper
import basic

/*
--------------
Storage Medium
--------------
*/
pub value struct inode{lno:int; loc:int}

pub struct fileSystem
  dir: list<(string, int)> = [("stdout", 0)];
  ilist:list<(int,inode)> = [(0,Inode(lno=0,loc=0))];
  dreg: list<(int,string)> = [(0,"")];
  dnext: int = 1;
  inext: int = 1

fun inode/show(inode: inode): string
  "(lno=" ++ inode.lno.show ++ ", loc=" ++ inode.loc.show ++ ")"

pub fun show(fs: fileSystem): string
  var buff := ""
  buff := buff ++ "dir: "   ++ fs.dir.show   ++ "\n"
  buff := buff ++ "ilist: " ++ fs.ilist.show ++ "\n"
  buff := buff ++ "dreg: "  ++ fs.dreg.show  ++ "\n"
  buff := buff ++ "dnext: " ++ fs.dnext.show ++ "\n"
  buff := buff ++ "inext: " ++ fs.inext.show ++ "\n"
  buff


/*
------------------------
File reading and writing
------------------------
*/

// write effect is defined in basic/write
fun fread(ino:int, fs:fileSystem): fail string
  val inode = fs.ilist.lookup-fail(ino)
  fs.dreg.lookup-fail(inode.loc)

fun fwrite(ino:int, cs:string, fs:fileSystem): fail fileSystem
  val loc = fs.ilist.lookup-fail(ino).loc
  val file = fs.dreg.lookup-fail(loc)
  val dreg = fs.dreg.modify(loc, file++cs)
  fs(dreg=dreg)



/*
--------------------------
File creating and opening
--------------------------
*/


fun fopen(fname:string, fs:fileSystem): fail int
  fs.dir.lookup-fail(fname)


fun fcreate(fname:string, fs: fileSystem): fail (int,fileSystem)
  if fs.dir.has(fname) then
    val ino = fopen(fname, fs)
    val inode = fs.ilist.lookup-fail(ino)
    val dreg' = fs.dreg.modify(inode.loc, "")
    (ino, fs(dreg=dreg'))
  else
    val loc = fs.dnext
    val dreg = [(loc, "")] ++ fs.dreg
    val ino = fs.inext
    val inode = Inode(loc=loc, lno=1)
    val ilist = [(ino, inode)] ++ fs.ilist
    val dir = [(fname, ino)] ++ fs.dir
    (ino, fs(dir=dir, ilist=ilist, dreg=dreg, dnext=fs.dnext+1, inext=fs.inext+1))


/*
--------------------------
File linking and unlinking
--------------------------
*/

fun flink(src:string, dest:string, fs: fileSystem): fail fileSystem
  if fs.dir.has(dest) then fail()
  val ino = fs.dir.lookup-fail(src)
  val inode = fs.ilist.lookup-fail(ino)
  val inode' = inode(lno=inode.lno+1)
  fs(
    dir = [(dest, ino)] ++ fs.dir,
    ilist = fs.ilist.modify(ino, inode')
  )

fun funlink(fname:string, fs:fileSystem)
  if fs.dir.has(fname) then
    val ino = fs.dir.lookup-fail(fname)
    val dir' = fs.dir.remove(fname)
    val inode = fs.ilist.lookup-fail(ino)
    if inode.lno > 1 then
      val inode' = inode(lno=inode.lno - 1)
      val ilist' = fs.ilist.modify(ino,inode')
      fs(dir=dir', ilist=ilist')
    else
      val ilist' = fs.ilist.remove(ino)
      val dreg' = fs.dreg.remove(inode.loc)
      fs(dir=dir', ilist=ilist', dreg=dreg')
  else fail()




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


pub fun fileIO(action: () -> <fileIO|e> a): e a
  var fs: fileSystem := FileSystem()
  with handler
    fun read(ino)
      with withDefault(Nothing)
      Just(fread(ino, fs))
    fun write(ino, cs)
      with withDefault(())
      fs := fwrite(ino, cs, fs)
    fun create(fname)
      with withDefault(Nothing)
      val (ino, fs_new) = fcreate(fname, fs)
      fs := fs_new
      Just(ino)
    fun open(fname)
      with withDefault(Nothing)
      Just(fopen(fname, fs))
    fun link(src, dest)
      with withDefault(())
      fs := flink(src, dest, fs)
    fun unlink(fname)
      with withDefault(())
      fs := funlink(fname, fs)
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
