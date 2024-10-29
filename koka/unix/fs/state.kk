module state

/*
--------------
Handling State
--------------
*/

pub effect cell<a>
  fun get(): a
  fun put(a:a): ()

pub fun runState(init:a, action: () -> <div,cell<a>|e> b): <div|e> (a,b)
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
alias directory = list<(string, int)>
alias dataRegion = list<(int,string)>
alias iList = list<(int,inode)>


pub struct fileSystem
  dir: directory = [("stdout", 0)];
  ilist:iList = [(0,Inode(lno=0,loc=0))];
  dreg: dataRegion = [(0,"")];
  dnext: int = 1;
  inext: int = 1

pub fun reveal(fs: fileSystem): console ()
  println(fs.dir)
  //println(fs.iList)
  println(fs.dreg)
  println(fs.dnext)
  println(fs.inext)
