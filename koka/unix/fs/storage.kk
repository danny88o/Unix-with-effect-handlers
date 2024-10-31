module fs/storage

/*
--------------
Storage Medium
--------------
*/
pub value struct inode{lno:int; loc:int}
alias directory = list<(string, int)>
alias dataRegion = list<(int,string)>
alias iList = list<(int,inode)>


pub struct fileSystem
  dir: directory = [("stdout", 0)];
  ilist:iList = [(0,Inode(lno=0,loc=0))];
  dreg: dataRegion = [(0,"")];
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
