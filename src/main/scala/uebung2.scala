class uebung2 {
  // Aufgabe 1:
  // Implementieren Sie eine Funktion or, die ein logisches oder repräsentiert. Sie
  // soll zwei Parameter x und y vom Typ Boolean als Parameter enthalten und ein Boolean
  // zurückgeben. Programmieren Sie die Funktion so, dass nur das erste Element x ausgewertet
  // wird. Wenn x ein true enthält und y einen fehlerhaften z.B. mit einer Exception endenden
  // Ausdruck, so kommt es trotzdem zu einem Ergebnis. Überprüfen Sie dies, in dem Sie der
  // Funktion Ausdrücke übergeben, die in einer Exception/Error enden. (Verwenden Sie bei der
  // Implementierung nicht die Scala oder-Funktion).
  def or(x:Boolean, y: => Boolean): Boolean =
    if (x)
      true
    else y

  //-----------------------------------------------------------------------------------------

  //Aufgabe 2: Gegeben sei der folgende Programmcode:
        // def myMethod(param:Int):String=
        //    if param<0 then "kleiner null"
        //    if param>0 then "größer null"
        //    else "null"
  //Die Methode soll eine Integerzahl in einen String übersetzen. Dabei wird überprüft, ob der
  //übergebene Parameter größer, kleiner oder gleich 0 ist und ein entsprechender Text zurück
  //gegeben.
  //Überlegen Sie, ob das Programm das gewünschte Ergebnis liefert und falls nicht, wie Sie
  //den Code ändern müssen, um das Ergebnis zu erhalten.
  def myMethod():String=
    if param<0 then
      "kleiner null"
    else
      if param>0 then
        "größer null"
      else "null"


  




}

def explode(): Boolean =
  throw new RuntimeException("Boom!")

  //change

@main def test(): Unit =
  val u = new uebung2
  println("Fall 1:")
  println(u.or(true, explode()))
  println("\nFall 2:")
  println(u.or(false, explode()))