package vorlesung

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



}
