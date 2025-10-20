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

    //Aufgabe 5: Schreiben Sie eine Funktion teiler(zahl:Int):Int, die den größten ganzzahligen
    //Teiler der übergebenen Zahl– kleiner als die Zahl selbst – berechnet. Das heißt, wird eine
    //Primzahl übergeben, so gibt die Funktion 1 zurück, ist es keine Primzahl den
    //entsprechenden Teiler
    def teiler(zahl: Int): Int = {
      @annotation.tailrec
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


  // Helper to test short-circuit evaluation
  def explode(): Boolean = throw new RuntimeException("Boom!")


}
