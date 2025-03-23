module fs/storage

import helper

/*
--------------
Storage Medium
--------------
*/

pub value struct inode{lno:int; loc:int}

pub struct fileSystem
  dir: list<(string, int)> = [("stdout", 0)];
  ilist:list<(int,inode)> = [(0, Inode(lno=0,loc=0))];
  dreg: list<(int,string)> = [(0, "")];
  dnext: int = 1;
  inext: int = 1

fun inode/show(inode: inode): string
  "(lno=" ++ inode.lno.show ++ ", loc=" ++ inode.loc.show ++ ")"

pub fun fileSystem/show(fs: fileSystem): string
  "dir: "   ++ fs.dir.show   ++ "\n" ++
  "ilist: " ++ fs.ilist.show ++ "\n" ++
  "dreg: "  ++ fs.dreg.show  ++ "\n" ++
  "dnext: " ++ fs.dnext.show ++ "\n" ++
  "inext: " ++ fs.inext.show ++ "\n"

/*
------------------------
File reading and writing
------------------------
*/

pub fun fread(fs:fileSystem, ino:int): fail string
  val inode = fs.ilist.lookup-fail(ino)
  fs.dreg.lookup-fail(inode.loc)

pub fun fwrite(fs:fileSystem, ino:int, cs:string): fail fileSystem
  val loc = fs.ilist.lookup-fail(ino).loc
  val file = fs.dreg.lookup-fail(loc)
  val dreg = fs.dreg.modify(loc, file++cs)
  fs(dreg=dreg)


/*
--------------------------
File creating and opening
--------------------------
*/


pub fun fopen(fs:fileSystem, fname:string): fail int
  fs.dir.lookup-fail(fname)


pub fun fcreate(fs: fileSystem, fname:string): fail (int,fileSystem)
  if fs.dir.has(fname) then
    val ino = fs.fopen(fname)
    val inode = fs.ilist.lookup-fail(ino)
    val dreg' = fs.dreg.modify(inode.loc, "")
    (ino, fs(dreg=dreg'))
  else
    val loc = fs.dnext
    val dreg' = Cons((loc, ""), fs.dreg)
    val ino = fs.inext
    val inode = Inode(loc=loc, lno=1)
    val ilist' = Cons((ino, inode), fs.ilist)
    val dir' = Cons((fname, ino), fs.dir)
    (ino, fs(dir=dir', ilist=ilist', dreg=dreg', dnext=fs.dnext+1, inext=fs.inext+1))


/*
--------------------------
File linking and unlinking
--------------------------
*/

pub fun flink(fs: fileSystem, src:string, dest:string): fail fileSystem
  if fs.dir.has(dest) then fail()
  val ino = fs.dir.lookup-fail(src)
  val inode = fs.ilist.lookup-fail(ino)
  val inode' = inode(lno=inode.lno+1)
  fs(
    dir = [(dest, ino)] ++ fs.dir,
    ilist = fs.ilist.modify(ino, inode')
  )

pub fun funlink(fs:fileSystem, fname:string): fail fileSystem
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

