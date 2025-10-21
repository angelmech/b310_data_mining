package uebung

import scala.annotation.tailrec

class Uebung2 {
    // Aufgabe 1:
    // Funktion "or", die nur das erste Argument auswertet (Short-Circuit Evaluation)
    def or(x: Boolean, y: => Boolean): Boolean = {
      if (x)
        true
      else
        y
    }

    // ---------------------------------------------------------------------------

    // Aufgabe 2:
    // Korrigierte Version von myMethod
    // Ursprünglicher Code hatte falsche if statements:
    // beim ersten if muss ein else folgen, sonst wird es ignoriert
    def myMethod(param: Int): String = {
      if (param < 0)
        "kleiner null"
      else if (param > 0)
        "größer null"
      else
        "null"
    }

    // ---------------------------------------------------------------------------

    // Aufgabe 3:
    def aufgabe3(): Int = {
      val x = {
        val offset = 1;
        {
          val x = 2
          val offset = 10
          x + offset
        } +
          {
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
        else
          if (zahl % i == 0)
            i
          else
            loop(i - 1)

      loop(zahl - 1)
    }

  //Aufgabe 6: Schreiben Sie eine Funktion, quersumme mit der folgenden Signatur: def
  //quersumme(zahl:Int):Int. Sie soll die Quersumme der Zahl berechnen, die an die Funktion
  //übergeben wurde.
  def quersumme(zahl:Int):Int = {
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
  def fibo(x: Int):Int = {
    if (x==0)
      0
    else if (x==1)
      1
    else fibo(x-1) + fibo(x-2)
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










  // zum testen der short-circuit evaluation
  def explode(): Boolean = throw new RuntimeException("Boom!")


}
