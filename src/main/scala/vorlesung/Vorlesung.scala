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




}
