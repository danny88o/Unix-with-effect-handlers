module unix


/* 
-----------
File System
-----------
*/

alias fileDesc = int;

effect bio 
  fun write(fileDesc: fileDesc, text: string): ()
  //fun  read(fileDesc: fileDesc): string


fun basicIO(action : () -> bio a ) : (a,string)
  var cs: string := ""
  with return(x) (x,cs)
  with fun write(fd,s) cs := cs ++ s
  action()

val stdOut: fileDesc = 1;

pub fun echo(cs: string): bio ()
  write(stdOut,cs)


/* 
----------
Exceptions
----------
*/ 

effect status
  ctl exit(code: int): a


pub fun status(action: () -> <status|e> a ): e int
  with ctl exit(code) code
  action()
  0


/*
--------------------------
User Specific Environments
--------------------------
*/

type user
  Alice
  Bob
  Root

fun show(u: user): string
  match u
      Alice -> "alice"
      Bob -> "bob"
      Root -> "root"

effect whoami
  fun ask(): string


fun env(u:user, action: () -> <whoami|e> a): e a 
  with fun ask() u.show
  action()


effect session
  ctl subUser(u: user): ()

pub fun sessionmgr(u: user, action: () -> <session,whoami|e> a): e a
  with env(u)
  with ctl subUser(switch-to)
    mask<whoami>
      with env(switch-to)
      resume(())
  action()

/*
---------
Processes
---------
*/

effect fork
  ctl fork(): bool

pub fun nondet(action: () -> <fork|e> a): e list<a>
  with handler
    return(x) [x]
    ctl fork() resume(False) ++ resume(True)
  action()

effect interrupt
  val interrupt:()

effect pState<a>
  ctl done(): a
  ctl paused(): a

// pub fun reifyProcess(action: () -> <pState|e> a): e pState<a>
//   with handler 
//     ctl done() return
//     ctl paused()  
