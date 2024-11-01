module pipe

import basic
import fs
import helper

/*
-----------------------------------------------------------------
piper
-----------------------------------------------------------------
*/

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


/*
------------------------------------------------------------------
GNU utils
------------------------------------------------------------------
*/

pub fun cat(fname: string): <fileIO, yield<char>, exit> ()
  match open(fname)
    Nothing   -> exit(1)
    Just(ino) -> match read(ino)
      Nothing  -> exit(1)
      Just(cs) -> 
        cs.iter(yield)
        yield('0')


pub fun head(n:int): <await<char>, yield<char>, div> ()
  if n == 0 then yield('0')
  else 
    val c = await()
    yield(c)
    match c
      '0'  -> ()
      '\n' -> head(n - 1)
      _    -> head(n)


fun pst(c:char, cs:string): <yield<string>, await<char>, div> ()
  match c
    '0'  -> 
      yield(cs)
      yield("0")
    '\n' -> 
      yield(cs)
      yield("\n")
      pst(await(), "")
    ' ' -> 
      yield(cs)
      pst(await(), "")
    c -> 
      pst(await(), cs ++ c.string)


pub fun paste(): <yield<string>, await<char>, exn, div> ()
  pst(await(), "")


pub fun sed(target: string, str':string): <await<string>, yield<string>, div> ()
  val str = await()
  if str == target then yield(str') else yield(str)
  sed(target, str')

fun freq'(str:string, tbl: list<(string, int)>): <await<string>, yield<list<(string, int)>>, div> ()
  match str
    "0" -> yield(tbl)
    str -> 
      val tbl' = 
        with withDefault([(str,1)] ++ tbl)
        val sum = tbl.lookup-fail(str)
        tbl.modify(str, sum + 1)
      freq'(await(), tbl')

pub fun freq(): <await<string>, yield<list<(string, int)>>, div> ()
  freq'(await(), [])

fun renderRow(tuple: (string, int)): string
  val (s,i) = tuple
  s ++ ":" ++ i.show ++ ";"

pub fun renderTable(): <await<list<(string, int)>>, basic/write|e> ()
  val tbl: list<(string, int)> = await()
  echo(tbl.map(renderRow).join)
