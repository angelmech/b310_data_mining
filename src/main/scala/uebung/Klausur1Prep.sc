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

