module utils/pipes

pub val terminator = "0"

pub effect await<b>
  ctl await(): b
pub effect yield<b> 
  ctl yield(b:b): ()

fun pipe(p: () -> <yield<b>, div|e> a, c: () -> <await<b>, div|e> a): <div|e> a
  with raw ctl await()
    copipe(fn(x) rcontext.resume-shallow(x), p)
  c()

fun copipe(c: b -> <await<b>, div|e> a, p: () -> <yield<b>, div|e> a): <div|e> a
  with raw ctl yield(y)
    pipe(fn() rcontext.resume-shallow(()), fn() c(y))
  p()

pub fun(||)(p: () -> <yield<b>, div|e> a, c: () -> <await<b>, div|e> a): (() -> <div|e> a)
  fn() pipe(p,c)