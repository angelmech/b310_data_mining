package vorlesung

import scala.annotation.tailrec

class Vorlesung {

  def multiple(x:Int):Int = {
    if (x<=0)
      0
    else
      if (x%3==0 || x%5==0)
        multiple(x-1) + x
      else multiple(x-1)
  }


  //newtons approximation procedure
  def iter_sqrt(x: Double, estimate:Double):Double = {
    def isGoodEnough: Boolean = {
      math.abs(estimate * estimate - x) < 0.0000001
    }
    def improve: Double = {
      (x / estimate + estimate) / 2
    }

    if (isGoodEnough) estimate
    else iter_sqrt(x, improve)
  }


  // Exercise 3_2 Folie 14, What is value of "result"?
  // Antwort: 16


  // ---------------------------
  // Aufgabe: Prüfen, ob eine Zahl Primzahl ist
  // ---------------------------
  def primeTest(x:Long):Boolean={
    if (x <= 1) false
    else {
      @tailrec
      def primeHelper(divisor: Long): Boolean = {
        if (divisor * divisor > x) true
        else if (x % divisor == 0) false
        else primeHelper(divisor - 1)
      }
      primeHelper(2)
    }
  }

  // ---------------------------
  // Aufgabe: N-te Primzahl berechnen
  // ---------------------------
  def nthPrime(n: Int): Long = {
    @tailrec
    def loop(current: Long, count: Int): Long = {
      if (count == n) current - 1
      else if (primeTest(current)) loop(current + 1, count + 1)
      else loop(current + 1, count)
    }
    loop(2, 0)
  }


}
