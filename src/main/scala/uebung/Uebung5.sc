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
val listC1=List(1,2,3,4)
val listC2=List("a","b","c")
//Schreiben Sie eine Funktion, die aus den beiden Listen ein kartesisches Produkt bildet.
//Ergebnis soll eine Liste von Tupeln sein, deren erstes Element aus l1 kommt und deren
//zweites aus l2. Verwenden Sie dafür nur Higher Order Functions.

//(element aus list 1, element aus list 2)
val res = listC1.flatMap(x => listC2.map(y => (x,y)))
//listC2.map(y => (x,y))) -> List( (x,"a"), (x,"b"), (x,"c") )



//---------------------------------------------------------------------------------------
// Aufgabe  2: Implementieren Sie die folgenden Aufgabenstellungen:
// a) Schreiben Sie eine Funktion moduloMap(l:List[Int], mod_value:Int):Map[Int,List[Int]], die
//aus einer Liste von Zahlen, eine Map erzeugt, deren Schlüssel ein Int-Wert ist, der sich aus
//der Modulo-Rechnung des Listenwertes mit mod_value ergibt. Zu den Schlüsselwerten
//werden dann alle Ints der Ausgangsliste innerhalb einer Liste gespeichert: z.B.:
val l= List(1,4,5,7,8,9)
// moduloMap(l,3) ergibt dann:
// Map(1 -> List(7, 4, 1), 2 -> List(8, 5), 0 -> List(9))
// Benutzen Sie dafür nur einen Aggregationsoperator!

//Put each number into a "bucket" depending on what the number becomes when you do number % mod_value
//also groupBy(x % mod_value)
def moduloMap(l:List[Int], mod_value:Int):Map[Int,List[Int]] =
  l.foldLeft(Map[Int, List[Int]]())((m,x) => m.updated(x % mod_value, x::m.getOrElse(x % mod_value, Nil)))
// base ist Empty Map, da ausgabe Map sein muss
// Maps sind immutable, nicht veränderbar, deswegen immer neue map machen mit updated
//
moduloMap(l,3)







//---------------------------------------------------------------------------------------
// b) Gegeben sei eine Liste von Wörtern. Schreiben Sie eine Funktion
//countLetters(l:List[String]):Map[Int,Int], die aus der Liste von Wörtern eine Map generiert, in
//der gespeichert wird, wie viele Wörter es mit einer entsprechenden Buchstabenzahl
//(Schlüssel) gibt: z.B.
val w=List("Hallo","das","sind","ein","paar", "Wörter")
// countLetters(w)
// ergibt: Map(5 -> 1, 3 -> 2, 4 -> 2, 6 -> 1)
// Benutzen Sie dafür nur eine Aggregationsfunktion

def countLetters(l:List[String]):Map[Int,Int] =
  l.foldLeft(Map[Int,Int]())((m,x) => m.updated(x.length, 1 + m.getOrElse(x.length,0))) // size würde auch gehen
countLetters(w)





//---------------------------------------------------------------------------------------
// c) Wandeln Sie die Funktion so um, dass nicht die Anzahl der Wörter gespeichert wird,
//sondern die Wörter selbst. Benutzen Sie nur eine Aggregationsfunktion.
def countLetters2(l:List[String]):Map[Int, List[String]] =
  l.foldLeft(Map[Int, List[String]]())((m,x) => m.updated(x.length, x::m.getOrElse(x.length, Nil)))
countLetters2(w)






//---------------------------------------------------------------------------------------
//  d) Schreiben Sie eine Funktion avgNumbers(l:List[Int]):Map[Boolean, Double]. Die Funktion
//soll aus der Liste die Durchschnittswerte der geraden und der ungeraden Zahlen bilden. Der
//Schlüsselwert soll dabei ein Boolean sein, der bei true alle geraden Werte zusammenfasst
//und false bei allen ungeraden: z.B.:
val listD2= List(1,4,5,7,8,9)
//avgNumbers(l) ergibt:
// Map(false -> 5.5, true -> 6.0)
def avgNumbers(l:List[Int]):Map[Boolean, Double] =
  //l.foldLeft(Map[Boolean, Double]())((m,x) => m.updated(x%2==0, hier iwie summe/anzahl+m.getOrElse(x%2==0, false)))
  //l.flatMap(x => if (x%2==0) then (true,x.toDouble) else (false, x.toDouble))
  l.groupBy(_%2==0).view.mapValues(x=>x.sum.toDouble/x.size).toMap

avgNumbers(listD2)











//Für Klausur:
//c) Gegeben seien die beiden folgenden Listen:
//val l1=List(1,2,3,4)
//val l2=List("a","b","c")
//Schreiben Sie eine Funktion, die aus den beiden Listen ein kartesisches Produkt bildet.
//Ergebnis soll eine Liste von Tupeln sein, deren erstes Element aus l1 kommt und deren
//zweites aus l2. Verwenden Sie dafür nur Higher Order Functions.

//Lösung:
//val l3= l1.flatMap(x=> l2.map(y=>(x,y)))






// Aufgabe 2: Implementieren Sie die folgenden Aufgabenstellungen:
//a) Schreiben Sie eine Funktion moduloMap(l:List[Int], mod_value:Int):Map[Int,List[Int]], die
//aus einer Liste von Zahlen, eine Map erzeugt, deren Schlüssel ein Int-Wert ist, der sich aus
//der Modulo-Rechnung des Listenwertes mit mod_value ergibt. Zu den Schlüsselwerten
//werden dann alle Ints der Ausgangsliste innerhalb einer Liste gespeichert: z.B.:
//val l= List(1,4,5,7,8,9)
//moduloMap(l,3) ergibt dann:
// Map(1 -> List(7, 4, 1), 2 -> List(8, 5), 0 -> List(9))
//Benutzen Sie dafür nur einen Aggregationsoperator!

//Lösung:
//def moduloMap(l:List[Int], mod_value:Int):Map[Int,List[Int]]=
// l.foldLeft(Map[Int,List[Int]]())((m,e)=> m.updated(e % mod_value, e::m.getOrElse(e %
//mod_value, List())))






//b) Gegeben sei eine Liste von Wörtern. Schreiben Sie eine Funktion
//countLetters(l:List[String]):Map[Int,Int], die aus der Liste von Wörtern eine Map generiert, in
//der gespeichert wird, wie viele Wörter es mit einer entsprechenden Buchstabenzahl
//(Schlüssel) gibt: z.B.:val w=List("Hallo","das","sind","ein","paar", "Wörter")
//countLetters(w)
//ergibt: Map(5 -> 1, 3 -> 2, 4 -> 2, 6 -> 1)
//Benutzen Sie dafür nur eine Aggregationsfunktion.

//Lösung:
//def countLetters(l:List[String]):Map[Int,Int]=
// l.foldLeft(Map[Int,Int]())((m,e)=>m.updated(e.size,1+m.getOrElse(e.size,0)))






//c) Wandeln Sie die Funktion so um, dass nicht die Anzahl der Wörter gespeichert wird,
//sondern die Wörter selbst. Benutzen Sie nur eine Aggregationsfunktion.

//Lösung:
//def countLetters2(l:List[String]):Map[Int,List[String]]=
// l.foldLeft(Map[Int,List[String]]())((m,e)=>m.updated(e.size,e::m.getOrElse(e.size,List[String]
//())))







//d) Schreiben Sie eine Funktion avgNumbers(l:List[Int]):Map[Boolean, Double]. Die Funktion
//soll aus der Liste die Durchschnittswerte der geraden und der ungeraden Zahlen bilden. Der
//Schlüsselwert soll dabei ein Boolean sein, der bei true alle geraden Werte zusammenfasst
//und false bei allen ungeraden: z.B.:
//val l2= List(1,4,5,7,8,9)
//avgNumbers(l) ergibt:
//Map(false -> 5.5, true -> 6.0)

//Lösung:
//def avgNumbers(l:List[Int]):Map[Boolean, Double]={
// l.groupBy(_%2==0).view.
// mapValues(x=>x.sum.toDouble/x.size).toMap
//}








