package uebung

import scala.annotation.tailrec
import scala.util.Random

class Uebung2 {
  // Aufgabe 1:
  // Funktion "or", die nur das erste Argument auswertet (Short-Circuit Evaluation)
  def or(x: Boolean, y: => Boolean): Boolean = {
    if (x)
      true
    else
      y
  }

  def ex: Boolean = throw new Error("Dies ist ein Fehler")
    //or(x = true, y = ex)
    //or(ex, y = true)


  // ---------------------------------------------------------------------------

  // Aufgabe 2:
  // Korrigierte Version von myMethod
  // ursprünglicher Code hatte falsche if statements:
  // beim ersten if muss ein else folgen, sonst wird es ignoriert

  //Es wird nicht das gewünschte Ergebnis zurückgeliefert, da das Erfüllen der ersten
  //Bedingung nicht zum Beenden der Funktion führt. Es müsste entweder die return-Anweisung
  //verwendet werden (imperativer Ansatz) oder ein else hinzugefügt werden (funktionaler
  //Ansatz). Wird ein else hinzugefügt, wird aus der gesamten Funktion ein Block gemacht, der
  //nur noch aus einer Anweisung besteht. Somit könnten auch die geschweiften Klammern
  //entfallen.
  def myMethod(param: Int): String =
    if (param < 0) "kleiner null"
    else if (param > 0) "größer null"
    else "null"


  // ---------------------------------------------------------------------------

  // Aufgabe 3:
  def aufgabe3(): Int = {
    val x = {
      val offset = 1;
      {
        val x = 2
        val offset = 10
        x + offset
      } + {
        val x = 5
        x + offset
      }
    }
    x
  }
  // x = 18
  // wenn semikolon weg, würde compiler x=val offset=1 lesen was quatsch wäre,
  // mit semikolon liest compiler val x=..., dann aber val offset=1; <- semikolon = ende des ausdrucks
  // also kompiliert er zuerst das, dann liest er für val x=... weiter

  //Musterlösung:
  //Die Sichtbarkeit von Variablen entspricht den in Programmiersprachen üblichen Regeln: Wird
  //ein Block definiert, so können die Variablen überschrieben werden, was dazu führt, dass die
  //äußeren Werte ihre Sichtbarkeit verlieren. Die im Block definierten Variablen verlieren ihre
  //Gültigkeit, sobald der Block beendet ist. Werden diese Regeln angewendet, so ergibt sich ein
  //Wert von 18 für die Variable x.
  //In Scala können Semikolons weggelassen werden. In diesem Fall jedoch nicht, weil der
  //Compiler dies für die Klasse Int als weiteren Parameter wertet. Dies liegt daran, dass es in
  //Scala multiple Parameterlisten gibt, was einfach nur bedeutet, dass eine Funktion nicht nur
  //eine Parameterliste haben kann. Zum Beispiel wie in der folgenden add-Funktion:
  //def add(x:Int)(y:Int):Int= x+y


  // ---------------------------------------------------------------------------

  // Aufgabe 4: Rekursion + Abbruchbedingung + Rückgabe des letzten gültigen Werts
  // Funktion squareUnder(x, max): quadriert x wiederholt, bis das Ergebnis > max ist
  @tailrec
  final def squareUnder(x: Double, max: Double): Double = {
    val squared = x * x
    if (squared > max)
      x
    else
      squareUnder(squared, max) // tail recursive call
  }

  // ---------------------------------------------------------------------------

  //Aufgabe 5:
  // Schreiben Sie eine Funktion teiler(zahl:Int):Int, die den größten ganzzahligen
  //Teiler der übergebenen Zahl– kleiner als die Zahl selbst – berechnet. Das heißt, wird eine
  //Primzahl übergeben, so gibt die Funktion 1 zurück, ist es keine Primzahl den
  //entsprechenden Teiler
  def teiler(zahl: Int): Int = {
    @tailrec
    def loop(i: Int): Int =
      if (i == 1)
        1
      else if (zahl % i == 0)
        i
      else
        loop(i - 1)

    loop(zahl - 1)
  }

  //Aufgabe 6: Schreiben Sie eine Funktion, quersumme mit der folgenden Signatur: def
  //quersumme(zahl:Int):Int. Sie soll die Quersumme der Zahl berechnen, die an die Funktion
  //übergeben wurde.
  def quersumme(zahl: Int): Int = {
    //quersumme ist z.b.: zahl=3160 --> quersumme = 3+1+6+0 = 10
    //bei negativer zahl: -57 -> 5+7 = 12
    val absolutZahl = math.abs(zahl)
    if (absolutZahl == 0)
      0
    else {
      (absolutZahl % 10) + quersumme(absolutZahl / 10)
      // durch modulo 10 bleibt letzte ziffer übrig
    }
  }


  //Aufgabe 7: Fibonacci -> fibo(X) funktion, die für eine beliebige Zahl X, die Fibonacci-Zahl
  //berechnet.
  // fibo: 0, 1, 1, 2, 3, 5, 8, 13, ...
  def fibo(x: Int): Int = {
    if (x == 0) 0
    else if (x == 1) 1
    else fibo(x - 1) + fibo(x - 2)
  }

  def fibo2(x: Int): BigInt = x match {
    case 0 => 0
    case 1 => 1
    case x => fibo2(x - 1) + fibo2(x - 2)
  }


  // Aufgabe 8: Wandeln Sie die Funktion aus Aufgabe 2 so um, dass der Aufruf der Funktion
  //fibo(100) zu einem richtigen Ergebnis kommt.
  def fiboTail(x: Int): Int = {
    @tailrec
    def loop(n: Int, current: Int, next: Int): Int = {
      if (n == 0)
        current
      else {
        loop(n - 1, current = next, next = current + next)
      }
    }
    loop(x, 0, 1)
  }
  // z.b.: fibo(5) → loop(5, 0, 1) , initial call:
  // 1st recursion: n != 0, also machen wir call: loop(n - 1, current = next, next = current + next)
  // jetzt haben wir loop(4, 1, 1)
  // after 2nd recursion: loop(3, 1, 2)
  // after 3rd rec.: loop(2, 2, 3)
  // after 4th rec.: loop(1, 3, 5)
  // after 5th rec.: loop(0, 5, 8)
  // loop(0, 5, 8) -> Base Case -> return current = 5
  // Fibonacci(x=5) = 5

//----------------------------------------------------------------------

  //Aufgabe 9: 2520 ist die kleinste Zahl, die durch jede Zahl von 1-10 ohne Rest geteilt werden
  //kann. Was ist die kleinste positive Zahl, die durch alle Zahlen von 1-20 ohne Rest teilbar ist?
  //(Projekt Euler Aufgabe 5)
  //Schreiben Sie eine Funktion, die in Abhängigkeit von einer Zahl X berechnet, welches die
  //kleinste Zahl ist, die durch alle Zahlen von 1..X ohne Rest teilbar ist.
  //(Als kleiner Tipp: Schreiben Sie erst eine Funktion, die testet, ob eine Zahl durch eine Menge
  //von Zahl teilbar ist oder nicht. Dann lassen rufen Sie die Funktion solange auf, bis sie einen
  //entsprechenden Wert gefunden haben.)
  //Die Zahl ist: 232792560


  //Die kleinste Zahl, die durch alle Zahlen von 1 bis X teilbar ist,
  //ist das kleinste gemeinsame Vielfache (kgV) dieser Zahlen.
  // kgv(a,b) = a*b/ggT(a,b)    ,   ggT = gcd(greatest common divisor)

  def searchNumber(maxDiv: Int): Int = {
    @tailrec
    def searchNumber2(maxDiv: Int, z: Int): Int = {
      if (checkDiv(maxDiv, z)) z
      else searchNumber2(maxDiv, z + 1)
    }

    @tailrec
    def checkDiv(maxnum: Int, number: Int): Boolean = maxnum match {
      case 1 => true
      case _ => if (number % maxnum == 0) checkDiv(maxnum - 1, number) else false
    }
    searchNumber2(maxDiv,1)
  }


  //----------------------------------------------------------------------

  //Aufgabe 10: Die Summe aller Primzahlen der Zahlen bis 10 ist: 2+3+5+7 =17. Schreiben Sie
  //eine Funktion, die die Summe aller Primzahlen unter 2 Millionen bildet (Ergebnis:
  //12272577818052).
  // das ist eig. die richtige Lösung -> 142913828922,
  // source: https://projecteuler.net/problem=10 + https://github.com/lucky-bai/projecteuler-solutions/blob/master/Solutions.md

  def summePrimzahlen(limit: Int): Long = {

    def istPrimzahl(n: Int): Boolean = {
      @annotation.tailrec
      def pruefeTeiler(teiler: Int): Boolean = {
        if (teiler * teiler > n) true
        else if (n % teiler == 0) false
        else pruefeTeiler(teiler + 1)
      }
      if (n < 2) false
      else pruefeTeiler(2)
    }

    @annotation.tailrec
    def summe(n: Int, akk: Long): Long = {
      if (n > limit)
        akk
      else if (istPrimzahl(n))
        summe(n + 1, akk + n)
      else summe(n + 1, akk)
    }
    summe(2, 0L)
  }

  //Musterlösung
  def sumPrimes(max: Int): BigInt = {
    // Check if a number is prime
    def is_prim(X: Int): Boolean = {
      @annotation.tailrec
      def calcPrim(X: Int, i: Int, Max: Int): Boolean = {
        if (i >= Max) true
        else if (X % i == 0) false
        else calcPrim(X, i + 1, Max)
      }
      calcPrim(X, 2, math.sqrt(X).toInt + 1)
    }

    // Recursively sum all primes up to 'max'
    @annotation.tailrec
    def helperPrim(z: Int, sum: BigInt): BigInt = z match {
      case 2 => sum + 2
      case _ =>
        if (is_prim(z)) helperPrim(z - 1, sum + z)
        else helperPrim(z - 1, sum)
    }

    helperPrim(max, 0)
  }


  //----------------------------------------------------------------------

  //Aufgabe 11: Implementieren Sie die Methode calculatePi, die auf Basis Zufall die Zahl Pi
  //ermittelt (Monte Carlo Algorithmus – Nachlesbar in Wikipedia) Verwenden Sie dabei keine
  //Variablen sondern nur Rekursionen. Zufallszahlen erzeugen Sie mit der Klasse Random, die
  //Funktion nextDouble enthält:
  //import scala.util.Random
  //val randGen= Random

  /**
   * Berechnet Pi mittels Monte-Carlo-Methode ohne Variablen, nur mit Rekursion.
   * @param n Anzahl der zu werfenden Punkte
   * @return Approximation von Pi
   */
  def calculatePi(n: Int): Double = {
    val randGen = new Random()

    @tailrec
    def loop(remaining: Int, inside: Int): Int = {
      if (remaining == 0) inside
      else {
        val x = randGen.nextDouble()
        val y = randGen.nextDouble()
        val hit = if (x * x + y * y <= 1) 1 else 0
        loop(remaining - 1, inside + hit)
      }
    }
    4.0 * (loop(n, 0).toDouble / n)
  }

}
