package uebung

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


}
