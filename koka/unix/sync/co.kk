module sync/co

pub effect co<a>
  ctl uFork(): int
  ctl wait(pid: int): a
  ctl uInterrupt(): ()
  ctl getPid():int




pub struct funcqueue<a> (
  enter: list<a> = [],
  out: list<a> = []
)

pub fun enqueue<a>(q:funcqueue<a>, x:a): funcqueue<a>
  q(enter = Cons(x, q.enter))

pub fun dequeue<a>(q:funcqueue<a>): div maybe<(a, funcqueue<a>)>
  match q
    Funcqueue(Nil, Nil) -> Nothing
    Funcqueue(in_q, Nil) -> dequeue(Funcqueue(Nil, in_q.reverse()))
    Funcqueue(in_q, Cons(x, xs)) -> Just((x, Funcqueue(in_q, xs)))


