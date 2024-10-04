module users

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