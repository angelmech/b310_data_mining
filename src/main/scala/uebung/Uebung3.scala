package uebung

import scala.annotation.tailrec

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





}
