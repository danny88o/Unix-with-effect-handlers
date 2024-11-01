module utils/paste

import utils/pipes

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