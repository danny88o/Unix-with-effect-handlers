module fs/file-lu

import helper/fail
import helper/state
import fs/storage

pub effect fun link(src: string, dest: string): ()
pub effect fun unlink(fname: string): ()

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

pub fun fileLU(action: () -> <link, unlink, cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with fun link(src, dest)
      with withDefault(())
      val fs = flink(src, dest, get())
      put(fs)
  with fun unlink(fname)
      with withDefault(())
      val fs = funlink(fname, get())
      put(fs)
  action() 

