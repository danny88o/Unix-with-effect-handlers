module basicIO

/* 
-----------
Basic IO
-----------
*/

pub alias fileDesc = int;

pub effect fun write(fileDesc: fileDesc, fileContent: string): ()


pub fun basicIO(action : () -> <write|e> a ) : e (a,string)
  var buffer: string := ""
  with handler
    return(x) (x,buffer)
    fun write(fd,s) buffer := buffer ++ s
  action()

pub fun echo(cs: string): write ()
  val stdOut: fileDesc = 0;
  write(stdOut,cs)
