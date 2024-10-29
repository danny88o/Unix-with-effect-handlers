

/*
Failure effect and its general use
*/

effect ctl fail(): a

fun withDefault(default:a, action: () -> <fail|e> a): e a
  with ctl fail() default
  action()

fun lookup<k>(l:list<(k, v)>, key:k, ?(==): (k,k) -> bool): fail v
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then v else lookup(xs, key)

fun modify(l:list<(k, v)>, key:k, s:v, ?(==) : (k,k) -> bool): fail list<(k, v)>
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then Cons((k,s),xs) else Cons((k,v), xs.modify(key,s))
