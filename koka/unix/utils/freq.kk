module utils/freq

import utils/pipes

import helper/fail
  

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

pub fun renderTable(): <await<list<(string, int)>>|e> string
  val tbl: list<(string, int)> = await()
  tbl.map(renderRow).join