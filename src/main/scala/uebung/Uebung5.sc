// 1.a) Gegeben sei eine Liste von beliebigen Zahlen. Schreiben Sie eine Funktion, die mittels
//eines Aggregationsoperators den Durchschnitt aller geraden Zahlen und den Durchschnitt
//aller ungeraden Zahlen. Dabei soll nur einmal durch die Liste gegangen werden.

// durchschnitt → gesamtwert der zahlen / Anzahl de zahlen
val listA = List(1,2,3,4,5,6,7,2,13) //unsere Liste
val funcA = listA.foldLeft((0,0,0,0))((func,elem) => { // tupel aus (sumEven, countEven, sumOdd, countOdd)
  if (elem % 2 == 0) (func._1 + elem, func._2 + 1, func._3, func._4) // wenn elem is even, addiere elem zu sumEven und mach countEven+1 (sumEven+elem, countEven+1, sumOdd, countOdd)
  else (func._1, func._2, func._3 + elem, func._4 + 1) // wenn elem is NOT even, addiere elem zu oddEven und mach countOdd+1 (sumEven, countEven, sumOdd+elem, countOdd+1)
})
val avg=(funcA._1.toDouble/funcA._2, funcA._3.toDouble/funcA._4)
// avg(sumEven/countEvent, sumOdd/countOdd)
//mit foldLeft durchgeht man List nur 1 mal



//---------------------------------------------------------------------------------------
// 1.b) Schreiben Sie eine Funktion, die in einer Liste von Zahlen alle Werte dupliziert (nicht
//verdoppelt). Verwenden Sie dafür nur Higher Order Functions.

/*Nur Higher-Order-Functions erlaubt
Das bedeutet:
    ✔ map → macht 1 Element zu 1 Element
    ✔ flatMap → macht 1 Element zu 0..n Elementen
    ✔ foldLeft
    ✔ reduce
    ✔ anonyme Funktionen*/
//aber keine For-Schleifen, var, mutable Lists.

val listB = List(1,2,3,4,5,6,7,2,13)
listB.flatMap(x => List(x,x)) // für jedes x muss (x, x)
//flatMap macht dann aus List(List(1,1), List(2,2), ...) => List(1,1,2,2,...)



//---------------------------------------------------------------------------------------
// 1.c) Gegeben seien die beiden folgenden Listen:
//val l1=List(1,2,3,4)
//val l2=List("a","b","c")
//Schreiben Sie eine Funktion, die aus den beiden Listen ein kartesisches Produkt bildet.
//Ergebnis soll eine Liste von Tupeln sein, deren erstes Element aus l1 kommt und deren
//zweites aus l2. Verwenden Sie dafür nur Higher Order Functions.


