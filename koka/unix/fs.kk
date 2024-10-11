module fs

effect cell<a>
  fun get(): a
  fun put(a:a): ()

pub fun runState(init:a, action: () -> <cell<a>,div|e> a): <div|e> (a,a)
  var curr := init
  with handler
    return(x)   (curr,x)
    fun get()   curr
    fun put(x)  curr := x
  action()  


alias directory = list<(string, int)>
value struct inode{lno:int; loc:int}
alias dataRegion = list<(int,string)>
alias iList = list<(int,inode)>

struct fileSystem
  dir: directory = [("stdout", 0)];
  ilist:iList = [(0,Inode(lno=0,loc=0))];
  dreg: dataRegion = [(0,"")];
  dnext: int = 1;
  inext: int = 1
    

effect fileIO
  fun read(inum:int): maybe<string>
  fun write(inum:int, content:string): ()
  
effect val fail: n

effect fun eq(x:b, y:b): bool

fun lookup-fail(l:list<(int, v)>, key:int): fail v
  match l.lookup(fn(x:int) x == key)
    Nothing -> fail()
    Just(v) -> v

fun modify-fail(l:list<(int, v)>, key:int, s:v): fail list<(int, v)>
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then Cons((k,s),xs)  else [(k,s)] ++ xs.modify-fail(key,s)
    

// fun fread(inum:int, fs:fileSystem): fail string 
//   match fs.ilist.lookup( fn(x:int) {x == inum})
//     Just(inode) -> 
//       match fs.ilist.lookup( fn(x:int) {x == inode.loc})
//         Just(content) -> content
//         Nothing -> 
//     Nothing -> ""
  
fun fread(ino:int, fs:fileSystem): fail string 
  fs.dreg.lookup-fail(fs.ilist.lookup-fail(ino).loc)

fun fwrite(ino:int, fs:fileSystem, content:string): fail fileSystem
  val loc = fs.ilist.lookup-fail(ino).loc
  val file = fs.dreg.lookup-fail(loc)
  val dreg = fs.dreg.modify-fail(loc, file)
  fs.copy(dreg = dreg)
  

