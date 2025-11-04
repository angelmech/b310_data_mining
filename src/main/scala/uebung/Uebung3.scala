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
  //Die Funktion soll eine bestimmte Anzahl des Strings „Hello “ auf die Konsole schreiben und
  //die Zeile am Ende der Ausgabe mit einem line feed beenden.
  //Schauen Code an und überlegen Sie, ob die Funktion auch das gewünschte Ergebnis liefert.
  //Falls nicht, überlegen Sie, wie Sie die Funktion anpassen müssen, damit eine entsprechende
  //Ausgabe erfolgt.

  //Lösung:
  //Hier wird beim Pattern Matching nicht mit einer Konstante gematcht sondern mit einer
  //Variable, d.h. diese Bedingung wird immer erfüllt. Dadurch kommt es nur zu einem println
  //und ein “Hello “ wird nicht ausgegeben. Um dies zu ändern, kann entweder die Variable n
  //groß geschrieben werden oder es wird ein Guard (if) eingebaut, der überprüft, ob (i==n) ist.

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

  /* ergebnis aus uebung2
  def quersumme(zahl:Int):Int={
    if(zahl==0)0
    else quersumme(zahl%10).abs + quersumme(zahl/10)
    // 6 + 3345
    // 5 + 334
    // 4 + 33
    // 3 + 3
    // 3 + 0
  }*/
  def quersumme(x:Int):Int= x match
  { case 0 => 0
    case _ => x % 10 + quersumme(x/10)
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

  // musterlösung:
  def searchNumber(maxDiv:Int):Int =
    searchNumber2(maxDiv,1) // Starte bei 1 und suche die erste Zahl, die alle Bedingungen erfüllt.

  @tailrec
  private def searchNumber2(maxDiv:Int, z:Int):Int=
    if (checkDiv(maxDiv,z)) //Prüft, ob z durch alle Zahlen von 1 bis maxDiv teilbar ist (mittels checkDiv).
      z
    else searchNumber2(maxDiv, z+1)

  //Diese Funktion prüft, ob number durch alle Zahlen maxnum, maxnum-1, …, 1 teilbar ist
  @tailrec
  private def checkDiv(maxnum:Int, number:Int):Boolean= maxnum match{
    case 1 => true //Basisfall: alles geprüft → true
    case _ => if (number % maxnum ==0) checkDiv(maxnum-1, number) //nicht teilbar → false
    else false //prüfe weiter mit maxnum - 1
  }
  //Bsp: checkDiv(3, 6) prüft:
  //6 % 3 == 0 → weiter
  //6 % 2 == 0 → weiter
  //6 % 1 == 0 → true → Ergebnis true

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

  val x= new Random
  val y= new Random

  def calculatePi(count:Int):Double= {
    val (in,out) = calculatePiHelper(count,0,0)
    in.toDouble/count*4
  }
  def calculatePiHelper(count:Int, in:Int, out:Int):(Int,Int)= {
    count match {
      case 0 => (in,out)
      case _ => {
        val x_coord = x.nextDouble
        val y_coord = y.nextDouble
        val hypothenuse= Math.sqrt((x_coord*x_coord)+(y_coord*y_coord))
        if (hypothenuse>1) calculatePiHelper(count-1, in,out+1)
        else calculatePiHelper(count-1, in+1,out)
      }
    }
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
    @tailrec
    def aggregate(list: IntList, sum: Int, count: Int):(Int,Int) = list match {
      case Empty => (sum, count)
      case Cons(h, t) => aggregate(t,sum + h, count + 1)
    }
    val (sum, count) = aggregate(l, 0, 0)
    sum.toDouble / count
  }

}
