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

val eof = "\x00"

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
  fn() {
    var str := ""
    with handler
      fun yield(s)
        if s == eof then echo(str) else str := str ++ s
    pipe(p,c)
  }

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
        yield('\x00')


pub fun head(n:int): <await<char>, yield<char>, div> ()
  if n == 0 then yield('\x00')
  else
    val c = await()
    yield(c)
    match c
      '\x00'  -> ()
      '\n' -> head(n - 1)
      _    -> head(n)


pub fun paste(): <await<char>, yield<string>, div> ()
  fun pst(c:char, cs:string): <yield<string>, await<char>, div> ()
    match c
      '\x00'  ->
        yield(cs)
        yield(eof)
      '\n' ->
        yield(cs)
        yield("\n")
        pst(await(), "")
      ' ' ->
        yield(cs)
        yield(" ")
        pst(await(), "")
      c ->
        pst(await(), cs ++ c.string)
  pst(await(), "")


pub fun sed(target: string, str':string): <await<string>, yield<string>, div> ()
  val str = await()
  if str == target then yield(str') else yield(str)
  sed(target, str')

alias table = list<(string, int)>

fun table/show(tbl: table): string
  fun showRow(tuple: (string, int)): string
    val (s,i) = tuple
    s ++ ":" ++ i.show ++ ";"
  tbl.map(showRow).join

pub fun freq(): <await<string>, yield<string>, div> ()
  fun freq'(str:string, tbl: table)
    if str == eof then
      yield(tbl.show)
      yield(eof)
    else
      val tbl' =
        with withDefault([(str,1)] ++ tbl)
        val sum = tbl.lookup-fail(str)
        tbl.modify(str, sum + 1)
      freq'(await(), tbl')
  freq'(await(), [])

pub fun rle(): <await<string>, yield<string>, div> ()
  fun rle'(input:string, curr:string, count:int): <await<string>, yield<string>, div> ()
    if input == eof then
      yield(count.show ++ "," ++ curr)
      yield(eof)
    elif curr == "" then
      rle'(await(), input, count)
    elif input == " " then
      rle'(await(), curr, count)
    elif input == curr then
      rle'(await(), curr, count + 1)
    else
      yield((count.show ++ "," ++ curr ++ " "))
      rle'(await(), input, 1)
  rle'(await(), "", 1)

pub fun unrle()
  fun unrle'(input:string)
    if input == eof then yield(eof)
    else
      val (n, str) = match input.split(",")
        Cons(n, Cons(str, Nil)) -> (n, str)
        Cons(n, Nil) -> (n, " ")
        _ -> ("1", input)
      match parse-int(n)
        Nothing -> yield(input)
        Just(n') -> yield(replicate(str, n').list/joinsep/join(" "))
    unrle'(await())
  unrle'(await())



pub fun ceaser(n:int)
  fun ceaser'(input:char)
    match input
     '\x00' -> yield('\x00')
     ' ' ->
      yield(' ')
      ceaser'(await())
     _   ->
      yield(char((input.int+n)%256))
      ceaser'(await())
  ceaser'(await())

pub fun hash(modulus:int)
  val base = 256
  fun hash'(c:char, value:int)
    if c == '\x00' then
      yield(value.show)
      yield(eof)
    else
      hash'(await(), (value * base + c.int) % modulus)
  hash'(await(), 0)

pub fun stdpipeout(action: () -> <yield<char>, basicIO/write|e> a): <basicIO/write|e> a
  with handler raw ctl yield(c:char)
    match c
      '\x00' -> rcontext.resume(())
      c   -> rcontext.resume(echo(c.string))
  action()

fun main()
  with runState(Filesystem())
  with fileIO
  with status
  (fn() echo("red red red green green green purple")) > "pic"
  ((fn() cat("pic")) || (fn() ceaser(1)) || paste || rle) > "pic.enc.rle"
  ((fn() cat("pic.enc.rle")) || paste || unrle) > "pic.enc"
  ((fn() cat("pic.enc")) || (fn() ceaser(-1)) || paste) >= "pic2"

