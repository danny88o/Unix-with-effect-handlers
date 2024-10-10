module io

/* 
-----------
File System
-----------
*/

pub alias fileDesc = int;

pub effect bio 
  fun write(fileDesc: fileDesc, fileContent: string): ()


pub fun basicIO(action : () -> <bio|e> a ) : e (a,string)
  var buffer: string := ""
  with handler
    return(x) (x,buffer)
    fun write(fd,s) buffer := buffer ++ s
  action()

pub fun echo(cs: string): bio ()
  val stdOut: fileDesc = 1;
  write(stdOut,cs)

