module fs

import basic
import helper

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
pub effect fun read(inum:int): maybe<string>
pub alias fileRW = <basic/write, read>

fun fread(ino:int, fs:fileSystem): fail string 
  val inode = fs.ilist.lookup-fail(ino)
  fs.dreg.lookup-fail(inode.loc)

fun fwrite(ino:int, cs:string, fs:fileSystem): fail fileSystem
  val loc = fs.ilist.lookup-fail(ino).loc
  val file = fs.dreg.lookup-fail(loc)
  val dreg = fs.dreg.modify(loc, file++cs) 
  fs(dreg=dreg)

pub fun fileRW(action: () -> <fileRW, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun read(ino)
    with withDefault(Nothing)
    Just(fread(ino,get()))
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

pub fun (>)(action: () -> <basic/write, create, exit|e> a, fname: string): <basic/write,create,exit|e> a
  val maybe-ino = create(fname)
    
  with override fun write(fileDesc, fileContent)
    match maybe-ino
        Nothing   -> exit(1)
        Just(ino) -> basic/write(ino, fileContent)
    
  action()


/*
--------------------------
FileIO
--------------------------
*/

pub alias fileIO = <basic/write, read, open, create, link, unlink>

pub fun fileIO(action: () -> <fileIO, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fileRW
  with fileCO
  with fileLU
  action()
  

pub fun cp(do-link: bool, src:string, dest:string): <basic/write, read, open, create, link, exit> ()
  if do-link then link(src,dest)
  else match open(src)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
        Nothing -> exit(1)
        Just(cs) -> (fn() {echo(cs)}) > dest

pub fun rm(fname:string): unlink ()
  unlink(fname)