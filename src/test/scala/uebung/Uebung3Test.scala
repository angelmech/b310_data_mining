package uebung

import org.scalatest.funsuite.AnyFunSuite

class Uebung3Test extends AnyFunSuite {

  val u = new Uebung3

  // Aufgabe 1
  test("celsius to other temps, pattern matching") {
    // Fahrenheit
    val fResult = u.convert("fahrenheit", 100)
    assert(fResult == ("fahrenheit", 212))

    // Reamur
    val rResult = u.convert("reamur", 100)
    assert(rResult == ("reamur", 80))

    // Kelvin
    val kResult = u.convert("kelvin", 0)
    assert(kResult == ("kelvin", 273))

    // Negative Celsius
    val negF = u.convert("fahrenheit", -40)
    assert(negF == ("fahrenheit", -40))

    // Unbekannte Einheit löst Exception aus
    assertThrows[IllegalArgumentException] {
      u.convert("unknown", 100)
    }
  }
}
