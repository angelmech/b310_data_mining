





// Aufgabe 3: Gegeben sei die folgende Liste von Tupeln:
val stundenProtokoll:List[(String, Int, List[Int])]= List(("Hans",1,List(7,9,4,12,8)),
("Hans",2,List(8,2,10,12,12)), ("Hans",3,List(8,8,8,7,9)),("Hans",4,List(8,9,10,9,8)),
("Monika",1,List(6,9,8,7,8)),("Monika",2,List(7,9,8,6,9)), ("Monika",3,List(12,9,12,8,7)),
("Monika",4,List(6,9)),("Kevin",1,List(6,9,8,7,8)),("Kevin",2,List(7,8,8,7,9)),
("Kevin",3,List(12,3,12,3,2)),("Kevin",4,List(12,3)))
//In der Liste wird aufgeführt, welche Mitarbeiter (Stelle 1), in welcher Kalenderwoche (Stelle
//2), wie viele Stunden (Stelle 2) gearbeitet hat. Die Stunden sind dabei als Liste repräsentiert
//– jeder Tag wird als Listeneintrag aufgeführt. Der Name identifiziert einen Mitarbeiter
//eindeutig, d.h. die Kombination mit Kalenderwoche
//Extrahieren Sie die folgenden Informationen aus der Liste. Benutzen Sie dabei nur Higher
//Order Functions.
//a) Schreiben Sie eine Funktion maxWorkPerWeek(l:List[(String, Int, List[Int])]):(String,Int,Int).
//Sie soll aus der Liste extrahieren, welcher Mitarbeiter in welcher Woche am meisten
//gearbeitet hat. Ergebnis soll ein Tripel sein, bestehend aus dem Namen, der Kalenderwoche
//und der Stundenanzahl.
//}
//b) Schreiben Sie eine Funktion maxWork(l:List[(String, Int, List[Int])]):(String,Int). Diese
//Funktion soll berechnen, wer insgesamt am meisten gearbeitet hat. Ergebnis soll ein Tupel
//sein, dass aus dem Namen und der Stundenanzahl besteht.

// a)
def maxWorkPerWeek(l:List[(String, Int, List[Int])]):(String,Int,Int) =
  l.map(x => (x._1, x._2, x._3.sum)).reduce((x,y) => if (x._3 < y._3) y else x)
maxWorkPerWeek(stundenProtokoll)

// b)
def maxWork(l:List[(String, Int, List[Int])]):(String,Int) =
  val totalHours = l.map(x => (x._1, x._3.sum))
    .groupBy(_._1).view
    .mapValues(_.map(_._2).sum)
  totalHours.reduce((x,y) => if (x._2<y._2) y else x)
maxWork(stundenProtokoll)