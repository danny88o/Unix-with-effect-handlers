module file-oc

import state
import fail

/*
-------------------------
File creation and opening
-------------------------
*/

pub effect fun create(fname: string): maybe<int>
pub effect fun open(fname: string): maybe<int>

fun fopen(fname:string, fs:fileSystem): fail int
  fs.dir.lookup(fname)

fun has(l:list<(k,v)>, key:k, ?(==): (k,k) -> bool): bool
  with withDefault(False)
  l.lookup(key)
  True
  
fun fcreate(fname:string, fs: fileSystem): fail (int,fileSystem)
  if fs.dir.has(fname) then
    val ino = fopen(fname, fs)
    val inode = fs.ilist.lookup(ino)
    val dreg = fs.dreg.modify(inode.loc, "")
    (ino, fs(dreg=dreg))
  else
    val loc = fs.dnext
    val dreg = [(loc, "")] ++ fs.dreg
    val ino = fs.inext
    val inode = Inode(loc, 1)
    val ilist = [(ino, inode)] ++ fs.ilist
    val dir = [(fname, ino)] ++ fs.dir
    (ino, fs(dir=dir, ilist=ilist, dreg=dreg, dnext=fs.dnext+1, inext=fs.inext+1))

pub fun fileCO(action: () -> <create, open|e> a): <cell<fileSystem>|e> a
  with fun create(fname)
    with withDefault(Nothing)
    val (ino, fs) = fcreate(fname, get())
    put(fs)
    Just(ino)
  with fun open(fname)
    with withDefault(Nothing)
    Just(fopen(fname, get()))

  with mask<cell<fileSystem>>
  action()