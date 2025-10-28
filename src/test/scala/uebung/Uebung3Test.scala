package uebung

import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}

class Uebung3Test extends AnyFunSuite {

  val u = new Uebung3

  // Aufgabe 1
  test("celsius to other temps, pattern matching") {
    val fResult = u.convert("fahrenheit", 100)
    assert(fResult == ("fahrenheit", 212))
    val rResult = u.convert("reamur", 100)
    assert(rResult == ("reamur", 80))
    val kResult = u.convert("kelvin", 0)
    assert(kResult == ("kelvin", 273))
    val negF = u.convert("fahrenheit", -40)
    assert(negF == ("fahrenheit", -40))
    assertThrows[IllegalArgumentException] {
      u.convert("unknown", 100)
    }
  }

  // Aufgabe 2
  test("convert Celsius to Fahrenheit, Reamur and Kelvin using tuple") {
    val fResult = u.convert(("fahrenheit", 100.0))
    assert(fResult == ("fahrenheit", 212.0))
    val rResult = u.convert(("reamur", 100.0))
    assert(rResult == ("reamur", 80.0))
    val kResult = u.convert(("kelvin", 0.0))
    assert(kResult == ("kelvin", 273.15))
    val negF = u.convert(("fahrenheit", -40.0))
    assert(negF == ("fahrenheit", -40.0))
    assertThrows[IllegalArgumentException] {
      u.convert(("unknown", 100.0))
    }
  }

  // Aufgabe 3
  test("printHello prints output manually observable") {
    u.printHello(3)
  }

  // Aufgabe 5
  test("kleinste teilbare zahl") {
    assert(u.euler5(20) === 232792560)
  }





}
