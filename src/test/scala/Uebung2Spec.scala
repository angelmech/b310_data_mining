import org.scalatest.funsuite.AnyFunSuite

class Uebung2Spec extends AnyFunSuite {

  val u = new Uebung2

  // Aufgabe 1
  test("or() should short-circuit when x = true") {
    assert(u.or(x = true, y = u.explode()) === true)
  }

  test("or() should evaluate y when x = false") {
    assertThrows[RuntimeException] {
      u.or(x = false, y = u.explode())
    }
  }

  // Aufgabe 2
  test("myMethod() should return correct strings") {
    assert(u.myMethod(-5) === "kleiner null")
    assert(u.myMethod(0) === "null")
    assert(u.myMethod(10) === "größer null")
  }

  // Aufgabe 3
  test("aufgabe3() should evaluate to 18") {
    assert(u.aufgabe3() === 18)
  }

  // Aufgabe 4
  test("squareUnder() should stop before exceeding max") {
    assert(u.squareUnder(2, 100) === 16.0)
  }

  // Aufgabe 5
  test("teiler() should return largest divisor smaller than the number") {
    assert(u.teiler(7) === 1)    // 7 is prime
    assert(u.teiler(9) === 3)    // 3 * 3 = 9
    assert(u.teiler(10) === 5)   // 5 divides 10
    assert(u.teiler(12) === 6)   // 6 divides 12
  }
}
