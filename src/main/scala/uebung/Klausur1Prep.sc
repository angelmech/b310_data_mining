// Aufgabe1: Berechne für jeden Mitarbeiter den durchschnittlichen
// Tagesarbeitswert über alle Wochen.
// Rückgabe: Map mit Namen → durchschnittliche Stunden.
val stundenProtokoll:List[(String, Int, List[Int])]= List(("Hans",1,List(7,9,4,12,8)),
  ("Hans",2,List(8,2,10,12,12)), ("Hans",3,List(8,8,8,7,9)),("Hans",4,List(8,9,10,9,8)),
  ("Monika",1,List(6,9,8,7,8)),("Monika",2,List(7,9,8,6,9)), ("Monika",3,List(12,9,12,8,7)),
  ("Monika",4,List(6,9)),("Kevin",1,List(6,9,8,7,8)),("Kevin",2,List(7,8,8,7,9)),
  ("Kevin",3,List(12,3,12,3,2)),("Kevin",4,List(12,3)))


//Aufgabe 1: Durchschnitt pro Mitarbeiter
//Schreibe eine Funktion:
//def avgWorkPerEmployee(l: List[(String, Int, List[Int])]): Map[String, Double]
//Berechne für jeden Mitarbeiter den durchschnittlichen Tagesarbeitswert über alle Wochen.
//Rückgabe: Map mit Namen → durchschnittliche Stunden.
def avgWorkPerEmployee(l: List[(String, Int, List[Int])]): Map[String, Double] =
  l.groupBy(_._1).view.mapValues(x => x.flatMap(_._3).sum.toDouble / x.flatMap(_._3).size).toMap
avgWorkPerEmployee(stundenProtokoll)

// Aufgabe 2: Anzahl Wochen pro Mitarbeiter
//Schreibe eine Funktion:
//def weeksPerEmployee(l: List[(String, Int, List[Int])]): Map[String, Int]
//Zähle, wie viele Wochen jeder Mitarbeiter im Protokoll hat.



//Aufgabe 3: Mitarbeiter mit allen Wochen > 40 Stunden
//def fullWeeks(l: List[(String, Int, List[Int])]): List[String]
//Finde alle Mitarbeiter, deren jede Woche mehr als 40 Stunden insgesamt hat.



//Aufgabe 4: Maximaler Tag pro Mitarbeiter
//def maxDailyPerEmployee(l: List[(String, Int, List[Int])]): Map[String, Int]
//Für jeden Mitarbeiter, finde den höchsten Wert, der an einem einzelnen Tag gearbeitet wurde.



//Aufgabe 5: Durchschnitt pro Tag
//def avgPerDay(l: List[(String, Int, List[Int])]): Map[Int, Double]
//Berechne für jeden Wochentag (1..5) den Durchschnitt über alle Mitarbeiter und Wochen.



//Aufgabe 6: Überstunden-Mitarbeiter
//def overtimeEmployees(l: List[(String, Int, List[Int])], limit: Int): List[String]
//Finde Mitarbeiter, die irgendwann mehr als limit Stunden an einem Tag gearbeitet haben.



//Aufgabe 7: Summe der Stunden pro Woche
//def sumPerWeek(l: List[(String, Int, List[Int])]): Map[Int, Int]
//Berechne, wie viele Stunden alle Mitarbeiter zusammen in jeder Woche gearbeitet haben.





val data: List[(String, Map[Int, List[Int]])] = List(
  ("Hans", Map(1 -> List(7,9,4), 2 -> List(8,2,10))),
  ("Monika", Map(1 -> List(6,9,8), 2 -> List(7,9,8))),
  ("Kevin", Map(1 -> List(6,9,8), 2 -> List(7,8,8)))
)

//Schreibe eine Funktion:
// def totalHoursPerEmployee(l: List[(String, Map[Int, List[Int]])]): Map[String, Int]
//Berechne die Gesamtstunden pro Mitarbeiter, über alle Wochen und Tage.



//Aufgabe 2: Maximaler Tageswert pro Woche über alle Mitarbeiter
//def maxDailyPerWeek(l: List[(String, Map[Int, List[Int]])]): Map[Int, (Int, List[String])]
//für jede Woche (Schlüssel in den Maps) sollst du herausfinden:
    //Die maximale Tagesstundenanzahl, die jemand gearbeitet hat.
    //Die Liste aller Mitarbeiter, die diesen Maximalwert in dieser Woche erreicht haben.
    //Ergebnis: Map von Woche → (max Stunden, Liste von Mitarbeitern)




// FOR + HOF training
val l= List(1,2)
val l2= List("a","b","c")
for (x <- l; y <- l2) yield (x,y) //kartesisches Produkt
l.flatMap(x => l2.map(y => (x,y)))

val db = List(
  ("francesco", "bloodsports"),
  ("simon", "jamesBond"),
  ("marcus", "jamesBond"),
  ("francesco", "die12KammernDerShaolin"),
  ("simon", "missionImpossible"),
  ("marcus", "die12KammernDerShaolin"),
  ("francesco", "missionImpossible"),
  ("gideon", "jamesBond")
)

//Finde alle Filme, die "francesco" gesehen hat.
for (x <- db if x._1 == "francesco") yield x._2
db.filter(_._1 == "francesco").map(_._2)


//Erstelle eine Liste aller Leute, die mehr als einen Film gesehen haben.
for ( (x,y) <- db.groupBy(_._1).toList if y.size > 1) yield x
db.groupBy(_._1).filter(_._2.size > 1).keys.toList
db.foldLeft(Map[String, Int]())((m,x) => m.updated(x._1, 1 + m.getOrElse(x._1,0))).filter(_._2 > 1).keys.toList


// Erstelle eine Liste von Tupeln (Name, List[Filme]) für jede Person.
for ((name,entries) <- db.groupBy(_._1).toList) yield (name, for((_,film) <- entries) yield film)
db.groupBy(_._1).map(x => (x._1, x._2.map(_._2))).toList


//Erstelle eine Liste von Tupeln (Name, List[Filme]) für jede Person,
// aber die Film-Liste soll keine Duplikate enthalten.
for ((name,entries) <- db.groupBy(_._1).toList) yield (name, for((_,film) <- entries) yield film)
db.groupBy(_._1).map(x => (x._1, x._2.map(_._2))).toList.distinct


// Prüfe, ob jede Person mindestens einen Film gesehen hat.
db.groupBy(_._1).forall(_._2.size > 1)

// Finde denjenigen/diejenige(n), die die meisten Filme gesehen haben.
for ((x,y) <- List(db.groupBy(_._1).maxBy(_._2.size))) yield x
db.groupBy(_._1).view.mapValues(_.size).maxBy(_._2)._1