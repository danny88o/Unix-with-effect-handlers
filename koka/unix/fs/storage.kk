module fs/storage

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
