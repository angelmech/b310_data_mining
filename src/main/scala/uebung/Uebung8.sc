// Aufgabe 1: Gegeben sei die folgende Liste, die ausdrückt, welche Programmiersprachen,
//welche Programmiersprachen welche Paradigmen unterstützen:
val Paradigmen=List(
  ("erlang", "funktional"),
  ("erlang", "logisch"),
  ("prolog", "logisch"),
  ("scala", "funktional"),
  ("scala", "objektorientiert"),
  ("scala", "logisch"),
  ("java","objektorientiert"))
// Erstellen Sie die folgenden Listen über die for-Schleife sowie über die Funktionen map, flatMap und filter:
// a) Eine Liste aller Programmiersprachen, die objektorientiert sind.
// b) Eine Liste aller Paradigmen der Sprachen erlang und java.
// c) Eine Liste aller Programmiersprachen, die mehr als ein Paradigma beinhaltet.
// d) Eine Liste von Tupeln, die als erstes Element die Programmiersprache enthält und als zweites Element eine Liste der Paradigmen.
// Bei allen Ergebnissen können Duplikate vorkommen. Sie müssen nicht gefiltert werden



//a
//ergebnis: List(scala, java)
for(x <- Paradigmen if x._2 == "objektorientiert") yield x._1
Paradigmen.filter(_._2=="objektorientiert").map(_._1)


//b
//ergebnis: List(funktional, logisch, objektorientiert)
for(x <- Paradigmen if x._1=="erlang" || x._1=="java") yield x._2
Paradigmen.filter(x=> x._1=="erlang"|| x._1=="java").map(_._2)


//c
//ergebnis: List(erlang, scala)
for(x <- Paradigmen; y <- Paradigmen if y._1 == x._1 && y._2 != x._2) yield x._1
Paradigmen.flatMap(x => Paradigmen.map(y=>(x,y))).filter(z => z._1._1==z._2._1 && z._1._2!=z._2._2).map(_._1._1)


//d
//ergebnis: List( (erlang, List(funktional, logisch)) ...) /
for(x <- Paradigmen) yield (x._1, for(y <- Paradigmen if y._1==x._1) yield y._2)
Paradigmen.map(x => (x._1, Paradigmen.filter(y=>x._1==y._1).map(_._2)))