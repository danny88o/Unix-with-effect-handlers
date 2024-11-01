module helper/list

pub fun iter(xs: list<a>, f: a -> e b): e ()
  xs.map(f)
  ()

pub fun alt/iter(ys:c, f: a -> e b, ?list: c -> list<a>): e ()
  ys.list.map(f)
  ()

pub fun concat(xs: list<string>): string
  match xs
    Nil -> ""
    Cons(s, ss) -> s ++ concat(ss)