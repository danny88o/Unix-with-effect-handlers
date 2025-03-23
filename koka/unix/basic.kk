module basic

import basicIO

/*
----------
Exceptions
----------
*/

pub effect ctl exit(code: int): ()

pub fun status(action: () -> <exit|e> a ): e int
  with ctl exit(code) code
  action()
  0

/*
---------
Processes
---------
*/

pub effect ctl fork(): bool

pub fun nondet(action: () -> <fork|e> a): e list<a>
  with handler
    return(x) [x]
    ctl fork() resume(True) ++ resume(False)
  action()


pub effect ctl interrupt():()

type pState<a,e>
  Done(ret: a)
  Paused(remaction: () -> e pState<a,e>)

fun reifyProcess(action: () -> <interrupt|e> a): e pState<a,e>
  with ctl interrupt()
      Paused(fn() resume(()))
  Done(action())

pub fun schedule(jobs: list<pState<a,<fork, div|e>>>): <div|e> list<a>
  fun sched(ps: list<pState<a,<fork,div|e>>>, done: list<a>): <div|e> list<a>
    match ps
      Nil -> done
      Cons(Done(ret),xs) -> sched(xs, done ++ [ret])
      Cons(Paused(action), xs) -> sched(xs ++ nondet(action), done)
  sched(jobs, [])

pub fun timeshare(action: () -> <fork,interrupt,div|e> a): <div|e> list<a>
  schedule([Paused(fn () reifyProcess(action))])

pub fun interruptWrite(action: () -> <interrupt, basicIO/write|e> a): <interrupt, basicIO/write|e> a
  with override fun write(fd,s)
    interrupt()
    write(fd,s)

  action()


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

pub fun sessionmgr2(u: user, action: () -> <subUser,ask|e> a): e a
  var curr := u
  with fun ask() curr.show
  with ctl subUser(switch-to)
    curr := switch-to
    resume(())
  action()

pub fun sessionmgr3(u: user, action: () -> <subUser,ask|e> a): e a
  with env(u)
  with ctl subUser(switch-to)
      with override fun ask() switch-to.show
      resume(())
  action()