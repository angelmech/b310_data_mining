package uebung

import scala.annotation.tailrec
import scala.util.Random

class Uebung3 {

  //Aufgabe 1: Schreiben Sie eine Funktion def convert(convertTo:String, temperature: Int):
  //(String, Int), die zwischen einer Temperatur in Grad Celsius in Fahrenheit, Reamur und
  //Kelvin umrechnen. Die Formeln hierfür finden Sie in Wikipedia unter dem Stichwort
  //„Temperatur“. Die Funktion hat zwei Eingabewerte haben: Der erste Parameter soll die
  //Einheit beinhalten, in die konvertiert werden soll und der zweite Parameter eine Temperatur
  //in Grad Celsius. Ergebnis soll ein Tupel sein, in dem die ursprüngliche Gradzahl steht sowie
  //die neue Einheit und der berechnete Wert. Verwenden Sie für die Fallunterscheidung Pattern
  //Matching.

  /**
   * Fahrenheit: Celsius * 9/5 + 32
   * Reamur: Celsius * 4/5
   * Kelvin: Celsius + 273.15
   *
   * @param convertTo die einheit in die konvertiert werden soll
   * @param temperature in celsius
   * @return tupel (gradzahl davor INT, neue einheit STRING, gradzahl converted INT)
   */
  def convert(convertTo:String, temperature: Int): (String,Int) = convertTo.toLowerCase() match {
    case "fahrenheit" => (convertTo, Math.round(temperature * 9/5 + 32))
    case "reamur" => (convertTo, Math.round(temperature * 4/5))
    case "kelvin" => (convertTo, Math.round(temperature + 273.15).toInt)
    case _ => throw new IllegalArgumentException(s"Unbekannte Einheit: $convertTo")
  }

  //----------------------------------------------------------------------------------------------------

  //Aufgabe 2: Wandeln Sie die Funktion aus 1 so um, dass sie der folgenden Signatur
  //entspricht: def convert(change:(String, Double)):(String, Double). Verwenden Sie für das
  //Extrahieren des Tupels ebenfalls Pattern Matching.
  def convert(change:(String, Double)):(String, Double) = change match {
    case (unit, temp) => unit.toLowerCase() match {
      case "fahrenheit" => (unit, temp * 9.0 / 5 + 32)
      case "reamur" => (unit, temp * 4.0 / 5)
      case "kelvin" => (unit, temp + 273.15)
      case _ => throw new IllegalArgumentException(s"Unbekannte Einheit: $unit")
    }
  }

  //----------------------------------------------------------------------------------------------------

  //Aufgabe 3: Gegeben sei der folgende Programmcode:
  //def printHello(n:Int):Unit= {
  // def loop(i:Int):Unit= i match{
  // case n => println("")
  // case _ => println("Hello "); loop(i+1)
  // }
  // loop(0)
  // }
  //Die Funktion soll eine bestimmte Anzahl des Strings “Hello “ auf die Konsole schreiben und
  //die Zeile am Ende der Ausgabe mit einem line feed beenden.
  //Schauen Code an und überlegen Sie, ob die Funktion auch das gewünschte Ergebnis liefert.
  //Falls nicht, überlegen Sie, wie Sie die Funktion anpassen müssen, damit eine entsprechende
  //Ausgabe erfolgt.

  //Lösung:
  //case n => in Scala bedeutet: match auf eine Variable n, nicht auf den Wert der äußeren n.
  //Das führt dazu, dass immer case n zutrifft, weil der Matcher die Variable bindet.
  //Ergebnis: die Schleife endet sofort → keine Ausgabe

  def printHello(n: Int): Unit = {
    @tailrec
    def loop(i: Int): Unit = i match {
      case _ if i>=n => println()
      case _ => println("Hello "); loop(i+1)
    }
    loop(0)
  }

  //----------------------------------------------------------------------------------------------------

  //Aufgabe 4: Schreiben Sie eine Funktion, quersumme mit der folgenden Signatur: def
  //quersumme(zahl:Int):Int. Sie soll die Quersumme der Zahl berechnen, die an die Funktion
  //übergeben wurde.

  def quersumme(zahl:Int):Int={
    // bsp. quersumme: 33456 -> 3+3+4+5+6
    if(zahl==0)0
    else quersumme(zahl%10).abs + quersumme(zahl/10)
    // 6 + 3345
    // 5 + 334
    // 4 + 33
    // 3 + 3
    // 3 + 0
  }

  //----------------------------------------------------------------------------------------------------

  //Aufgabe 5: 2520 ist die kleinste Zahl, die durch jede Zahl von 1-10 ohne Rest geteilt werden
  //kann. Was ist die kleinste positive Zahl, die durch alle Zahlen von 1-20 ohne Rest teilbar ist?
  //(Projekt Euler Aufgabe 5)
  //Schreiben Sie eine Funktion, die in Abhängigkeit von einer Zahl X berechnet, welches die
  //kleinste Zahl ist, die durch alle Zahlen von 1..X ohne Rest teilbar ist.
  //(Als kleiner Tipp: Schreiben Sie erst eine Funktion, die testet, ob eine Zahl durch eine Menge
  //von Zahl teilbar ist oder nicht. Dann lassen rufen Sie die Funktion solange auf, bis sie einen
  //entsprechenden Wert gefunden haben.)
  def euler5(X: Int): Long = {
    /**
     * Checks whether a given number `n` is divisible by all integers
     * from 1 to X.
     */
    def isDivisibleByAll(n: Long): Boolean = {
      @tailrec
      def checkDivisor(d: Int): Boolean = {
        if (d > X) true // base case: all divisors checked successfully
        else if (n % d != 0) false // found divisor that doesn't divide evenly
        else checkDivisor (d + 1) // check next divisor
    }
      checkDivisor(1)
    }

    /**
     * Recursively searches for the smallest number divisible by all 1..limit.
     * Starts at `limit` and increases in steps of `limit` for efficiency.
     */
    @tailrec
    def loop(n: Long): Long =
      if (isDivisibleByAll(n)) n
      else loop(n + X)

    // Start search at 'limit' (the smallest possible candidate)
    loop(X)
  }

  //----------------------------------------------------------------------------------------------------

  //Aufgabe 6: Implementieren Sie die Methode calculatePi, die auf Basis Zufall die Zahl Pi
  //ermittelt (Monte Carlo Algorithmus – Nachlesbar in Wikipedia) Verwenden Sie dabei keine
  //Variablen sondern nur Rekursionen. Zufallszahlen erzeugen Sie mit der Klasse Random, die
  //Funktion nextDouble enthält:
  //import scala.util.Random
  // val x= new Random
  // val y= new Random
  //Verwenden Sie für die Lösungen nur Elemente aus der Funktionalen Programmierung, d.h.
  //hier nur unveränderliche Variablen und Rekursionen.

  /**
   * Berechnet Pi mittels Monte-Carlo-Methode ohne Variablen, nur mit Rekursion.
   *
   * @param n Anzahl der zu werfenden Punkte
   * @return Approximation von Pi
   */
  def calculatePi(n: Int): Double = {
    val rand = new Random()

    @tailrec
    def loop(remaining: Int, inside: Int): Int = {
      if (remaining == 0) inside
      else {
        val x = rand.nextDouble()
        val y = rand.nextDouble()
        val hit = if (x * x + y * y <= 1) 1 else 0
        loop(remaining - 1, inside + hit)
      }
    }

    4.0 * (loop(n, 0).toDouble / n)
  }

  //----------------------------------------------------------------------------------------------------

  //Aufgabe 7: Gegeben sei die folgende grundlegende Definition einer Liste von Integern
  //(IntList). In der Datenstruktur werden Zahlen (Integer) als verkettete Liste gespeichert. Sie
  //enthält die Operationen isEmpty, head, tail, prefix, map und flatMap mit den folgenden
  //Implementierungen:
  //abstract class IntList{
  //    def isEmpty:Boolean
  //    def head:Integer
  //    def tail:IntList
  //    def prefix(elem:IntList):IntList= elem match {
  //            case Empty => this
  //            case Cons(h,t) => Cons(h, prefix(t))
  //    }
  //}
  //
  //case object Empty extends IntList{
  //    def isEmpty = true
  //    def head= throw new Error ("List is Empty")
  //    def tail= throw new Error ("List is Empty")
  //}
  //case class Cons(head:Integer, tail:IntList) extends IntList{
  //    def isEmpty= false
  //}

  //a) Fügen Sie Klasse IntList in ein Worksheet ein und schreiben Sie die Funktion prefix so
  //um, dass Sie in den Klassen Empty und Cons implementiert ist. Das IntelliJ-Worksheet
  //erkennt dies sonst als fehlerhaft, was es aber nicht ist.

  abstract class IntList {
    def isEmpty: Boolean
    def head: Integer
    def tail: IntList
    def prefix(elem: IntList): IntList
  }

  case object Empty extends IntList {
    def isEmpty = true
    def head = throw new Error("List is Empty")
    def tail = throw new Error("List is Empty")
    def prefix(elem: IntList): IntList = elem
  }

  case class Cons(head: Integer, tail: IntList) extends IntList {
    def isEmpty = false
    def prefix(elem: IntList): IntList = elem match {
      case Empty => this
      case Cons(h, t) => Cons(h, prefix(t))
    }
  }

  //b) Schreiben Sie eine Funktion def average(l:IntList):Double, die aus einer Liste von Zahlen
  //einen Mittelwert bildet. Tipp: Aggregieren sie die Summe und die Anzahl einfach über ein
  //Tupel vom Typ (Int,Int).

  // Bsp.:
  // val l = Cons(3, Cons(5, Cons(7, Empty)))
  // average = (3+5+7) / 3 = 5.0
  def average(l:IntList):Double = {
    def aggregate(list: IntList):(Int,Int) = list match {
      case Empty => (0,0)
      case Cons(head,tail) =>
        // rekursiv: berechne Summe und Anzahl für den Rest der Liste
        val (sumTail,countTail) = aggregate(tail)
        // füge das aktuelle Element hinzu und gib das neue (sum, count)-Tupel zurück
        (head + sumTail, 1 + countTail) // Rückgabewert des gesamten case head + sumTail → die neue Summe, 1 + countTail → die neue Anzahl
    }

    val (sum, count) = aggregate(l)
    if (count==0) 0.0 //Schutz gegen eine leere Liste (Empty)
    else sum.toDouble / count // durchschnitt berechnen in Gleitkommadivision
  }

}
