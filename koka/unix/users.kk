module users

/*
--------------------------
User Specific Environments
--------------------------
*/

pub type user
  Alice
  Bob
  Root

fun show(u: user): string // .toString()
  match u
    Alice -> "alice"
    Bob -> "bob"
    Root -> "root"


effect fun ask(): string

pub fun whoami(): ask string
  ask()

fun env(u:user, action: () -> <ask|e> a): e a 
  with fun ask() u.show
  action()


effect ctl subUser(u: user): ()

pub fun switchUser(u: user): subUser ()
  subUser(u)

pub fun sessionmgr(u: user, action: () -> <subUser,ask|e> a): e a
  with env(u)
  with ctl subUser(switch-to)
    mask<ask>
      with env(switch-to)
      resume(())
  action()