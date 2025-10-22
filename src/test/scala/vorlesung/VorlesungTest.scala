package vorlesung

import org.scalatest.funsuite.AnyFunSuite
class VorlesungTest extends AnyFunSuite {

  val v = new Vorlesung

  test("newton approx") {
    assert(v.iter_sqrt(2,1) === 1.4142135623746899)
  }

}
