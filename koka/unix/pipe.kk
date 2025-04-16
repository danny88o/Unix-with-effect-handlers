module pipe

import basic
import basicIO
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

pub fun(||)(
  p: () -> <yield<b>, yield<string>, basicIO/write,  div|e> a,
  c: () -> <await<b>, yield<string>, basicIO/write,  div|e> a
  ): (() -> <basicIO/write,  div|e> a)
  fn()
    with fun yield(s)
      match s
        "0" -> ()
        _   -> echo(s)
    pipe(p,c)


/*
------------------------------------------------------------------
GNU utils
------------------------------------------------------------------
*/

pub fun cat(fname: string): <open, fs/fs/read, yield<char>, exit> ()
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


pub fun paste(): <yield<string>, await<char>, div> ()
  fun pst(c:char, cs:string): <yield<string>, await<char>, div> ()
    match c
      '0'  ->
        yield(cs)
        yield("0")
      '\n' ->
        yield(cs)
        yield("\n")
        pst(await(), " ")
      ' ' ->
        yield(cs)
        pst(await(), " ")
      c ->
        pst(await(), cs ++ c.string)
  pst(await(), "")


pub fun sed(target: string, str':string): <await<string>, yield<string>, div> ()
  val str = await()
  if str == target then yield(str') else yield(str)
  sed(target, str')


pub fun freq(): <await<string>, yield<list<(string, int)>>, div> ()
  fun freq'(str:string, tbl: list<(string, int)>): <await<string>, yield<list<(string, int)>>, div> ()
    match str
      "0" -> yield(tbl)
      str ->
        val tbl' =
          with withDefault([(str,1)] ++ tbl)
          val sum = tbl.lookup-fail(str)
          tbl.modify(str, sum + 1)
        freq'(await(), tbl')
  freq'(await(), [])


pub fun renderTable(): <await<list<(string, int)>>, yield<string>|e> ()
  fun renderRow(tuple: (string, int)): string
    val (s,i) = tuple
    s ++ ":" ++ i.show ++ ";"

  val tbl: list<(string, int)> = await()
  yield(tbl.map(renderRow).join)

pub fun stdpipeout(action: () -> <yield<char>, basicIO/write|e> a): <basicIO/write|e> a
  with handler raw ctl yield(c:char)
    match c
      '0' -> rcontext.resume(())
      c   -> rcontext.resume(echo(c.string))
  action()

fun main()
  with runState(Filesystem())
  with fileIO
  with status
  with stdpipeout
  (fn() echo("Hello World \n 88o")) > "hello.txt"
  ((fn () cat("hello.txt")) || (fn() head(1)) || paste) >= "pls.txt"

  (fn() echo("Goodby Friend")) > "bye.txt"
  (fn() cat("bye.txt")) > "carp.txt"
