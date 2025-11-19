List(1,2,3).foldRight("")((x: Int, y: String) => x.toString ++ y)

List(1,2,3).foldLeft("")((x: String, y:Int) => x ++ y.toString)
// "" is base value + defines what type the result should be, here String

val v1=Array(1,2,3)
val v2=Array(4,5,6)
def aggArray(v1: Array[Int], v2:Array[Int]):Array[Int] = {
  v1.zip(v2).map(x => x._1 + x._2) // ._1 -> first element of pair
  // v1.zip(v2).map { case (a, b) => a + b }
}

// option mit None,Some statt exception handling

// val ist wie final in java, immutable
// var ist mutable

// bsp: var l = list(1,2,3)
// l ist ein zeiger auf eine liste im speicher, liste wird nun mit 0::list erweitert, d.h
// neues "array" im speicher
// man verändert jetzt referenz(l) auf speicher

val l= List(1,2,3,4,5,6)
l.groupBy(_ %3) //→ Map( 2 -> List(2, 5),
                      // 1 -> List(1, 4),
                      // 0 -> List(3, 6))

// variant 1: mutable map with for loop
def groupBy[T, U](in: Iterable[T], f: T => U):Map[U,List[T]]={
  val res= scala.collection.mutable.Map[U,List[T]]()
  for (el <- in) {
    val groupByValue= f(el)
    res.update(groupByValue, el::res.getOrElse(groupByValue,List())) // falsche reihenfolge
    // erzeugt veränderung im inneren des objektes
  }
  res.toMap.view.mapValues(_.reverse).toMap // deswegen hier reverse
}

// variant 2: immutable map with for loop
def groupBy[T, U](in: Iterable[T], f: T => U):Map[U,List[T]] = {
  var res= Map[U,List[T]]()
  for (el <-in) {
    val groupByValue= f(el)
    res= res.updated(groupByValue, el::res.getOrElse(groupByValue,List()))
    // updated erzeugt neues objekt, setzt zeiger/referenz neu
  }
  res.view.mapValues(_.reverse).toMap
}

def groupBy[T,U](in: Iterable[T], f: T => U):Map[U,List[T]] = {
  in.foldLeft( Map[ U, List[T] ] () ) { // das ist der base-Wert , hier startet man mit einem leeren Wert
    (map, elem) =>
      val groupByVal = f(elem)
      map.updated(groupByVal, elem::map.getOrElse(groupByVal, List() ))
      // updaten an stelle vom groupBy value, packen element dann rein, getorelse check
  }.view.mapValues(_.reverse).toMap
}


// update , updated -> erzeugt neues obj. unterschied

def flatten(l:List[Any]):List[Any] = l match {
  case Nil => Nil
  case (head:List[_])::tail => flatten(head) ++ flatten(tail) // head must be list
  case (head::tail) => head::flatten(tail) // head can be any type
}



// myFlatten vorteil folie 9 -> obere lösung checkt den Typ beim compilen
// und wirft fehler -> besser

val l = List(1,2,3)
val l2 = List("a","b","c")
l.map(x => l2.map(y=>(x,y))).flatten


val db =List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus",
  "jamesBond"), ("francesco", "die12KammernDerShaolin"))

for(x <- db if x._1=="francesco") yield x._2 // gib filme, die francesco gesehen hat
db.filter(x => x._1=="francesco").map(_._2)


for (x <- db; y <- db if (x._1==y._1 && x._2!=y._2)) yield x._1
db.flatMap( x=> db.map(y=> (x,y))).filter(z=>z._1._1== z._2._1 && z._1._2!=z._2._2).map(_._1._1)

//Create two functions that determine who have seen what films
//The result should contain a list of tuples which contain the name of each person
// as the fist element and a list of all films the person has seen as the second
for (x <- db) yield (x._1, for (y <-db if y._1==x._1 ) yield y._2)
db.map(x=> (x._1, db.filter(y=> x._1 == y._1).map(_._2)))





