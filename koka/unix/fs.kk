module fs


/*
--------------
Handling State
--------------
*/

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


/*
--------------
Storage Medium
--------------
*/

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
    

/*
Failure effect and its general use
*/

effect ctl fail(): a

fun lookup<k>(l:list<(k, v)>, key:k, ?(==): (k,k) -> bool): fail v
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then v else lookup(xs, key)

fun modify(l:list<(k, v)>, key:k, s:v, ?(==) : (k,k) -> bool): fail list<(k, v)>
  match l
    Nil -> fail()
    Cons((k,v), xs) -> if k==key then Cons((k,s),xs)  else Cons((k,v), xs.modify(key,s))

/*
------------------------
File reading and writing
------------------------
*/

effect fileIO
  fun read(inum:int): maybe<string>
  fun write(inum:int, content:string): ()

fun fread(ino:int, fs:fileSystem): fail string 
  fs.dreg.lookup(fs.ilist.lookup(ino).loc)

fun fwrite(ino:int, fs:fileSystem, content:string): fail fileSystem
  val loc = fs.ilist.lookup(ino).loc
  val file = fs.dreg.lookup(loc)
  val dreg = fs.dreg.modify(loc, file) 
  fs(dreg=dreg)
  
fun withDefault(default:a, action: () -> <fail|e> a): e a
  with ctl fail() default
  action()

fun fileRW(action: () -> <fileIO,cell<fileSystem>|e> a): <cell<fileSystem>|e> a
  with handler
    fun read(inum:int)
      with withDefault(Nothing)
      Just(fread(inum,get()))

    fun write(inum:int, content:string)
      with withDefault(())
      put(fwrite(inum, get(), content))

  action() 

/*
-------------------------
File creation and opening
-------------------------
*/

effect fileCO
  fun create(fname: string): maybe<int>
  fun open(fname: string): maybe<int>

fun fopen(fname:string, fs:fileSystem): fail int
  fs.dir.lookup(fname)

fun has(l:list<(k,v)>, key:k, ?(==): (k,k) -> bool): bool
  with withDefault(False)
  l.lookup(key)
  True
  
// fun fcreate(fname:string, fs: fileSystem):fail (int,fileSystem)
//   if fs.dir.has(fname) then
//     val ino = fopen(fname, fs)
//     val inode = fs.ilist.lookup(ino)
//     val dreg = fs.dreg.modify(inode.loc, "")
//     (ino, fs(dreg=dreg))
//   else
//     val dreg = fs.dreg ++ [(fs.inext, "")]
//     ()  

