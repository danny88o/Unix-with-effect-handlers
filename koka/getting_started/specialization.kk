module specialization

type colour  
  Red
  Black

type tree<k,a> 
  Leaf
  Node(colour : colour, left : tree<k,a>, key : k, value : a, right : tree<k,a>)


fun fold(t : tree<k,a>, acc : b, f : (k, a, b) -> b) : b
  match t
    Node(_,l,k,v,r) -> r.fold( f(k,v,l.fold(acc,f)), f)
    Leaf -> acc

fun main() 
  val t = Node(Red, Node(Red, Leaf, 2, True, Leaf), 2, True, Leaf)
  val count = t.fold(0, fn(k,v,acc) if v then acc+1 else acc);
  println(count.show)
