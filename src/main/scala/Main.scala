import scala.annotation.tailrec

object Main {

  private class Uebung2 {

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
    // wenn semikolon weg, würde compiler x = val offset = 1 lesen was quatsch wäre,
    // mit semikolon liest compiler val x =.., dann aber val offset =1; <- semikolon = ende des ausdrucks
    // also ´kompiliert er zuerst das, dann liest er für val x =... weiter


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
  }
















  // ---------------------------------------------------------------------------

  // Hilfsfunktion, die einen Fehler wirft
  private def explode(): Boolean = {
    throw new RuntimeException("Boom!")
  }

  // ---------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    val u = new Uebung2

    println("---------Aufgabe 1----------")
    println("Fall 1:")
    println(u.or(x = true, y = explode())) // sollte true liefern, ohne Exception
    println("\nFall 2:")
    try {
      println(u.or(x = false, y = explode())) // sollte Exception werfen
    } catch {
      case e: Exception => println("Exception gefangen: " + e.getMessage)
    }

    println("\n---------Aufgabe 2----------")
    println("param = -3 → " + u.myMethod(-3))
    println("param = 0 → " + u.myMethod(0))
    println("param = 5 → " + u.myMethod(5))

    println("\n---------Aufgabe 3----------")
    println("val x = " + u.aufgabe3())

    println("\n---------Aufgabe 4----------")
    println("squareUnder(2, 100) = " + u.squareUnder(2, 100))
  }
}
