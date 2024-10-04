module fs

/* 
-----------
File System
-----------
*/

alias fileDesc = int;

effect write 
  fun write(fileDesc: fileDesc, fileContent: string): ()


pub fun basicIO(action : () -> write a ) : (a,string)
  var cs: string := ""
  with handler
    return(x) (x,cs)
    fun write(fd,s) cs := cs ++ s
  action()

val stdOut: fileDesc = 1;
pub fun echo(cs: string): write ()
  write(stdOut,cs)

