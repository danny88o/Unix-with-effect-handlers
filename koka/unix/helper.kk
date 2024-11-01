/*
Deafult effect and/with methods on lists
*/

pub effect ctl fail(): a

pub fun withDefault(default:a, action: () -> <fail|e> a): e a
  with ctl fail() default
  action()

pub fun lookup-fail<k>(l:list<(k, v)>, key:k, ?(==): (k,k) -> bool): fail v
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then v else xs.lookup-fail(key)

pub fun modify(l:list<(k, v)>, key:k, s:v, ?(==) : (k,k) -> bool): fail list<(k, v)>
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then Cons((k,s),xs) else Cons((k,v), xs.modify(key,s))

pub fun has(l:list<(k,v)>, key:k, ?(==): (k,k) -> bool): bool
  with withDefault(False)
  l.lookup-fail(key)
  True

pub fun remove(l: list<(k, v)>, key: k, ?(==): (k,k) -> bool): list<(k, v)>
  match l
    Nil -> Nil
    Cons((k,v), xs) -> if k==key then xs else Cons((k,v), xs.remove(key))

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


