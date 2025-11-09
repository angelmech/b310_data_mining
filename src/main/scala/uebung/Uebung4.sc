//Aufgabe 1: Mengen werden üblicherweise durch die Aufzählung ihrer Elemente
//implementiert. Sie lassen sich aber auch über Mengenoperationen definieren. Hier wird eine
//Menge repräsentiert über eine Funktion contains:Int => Boolean, die aussagt, ob ein Element
//in der Menge ist oder nicht.
//Die Menge wird über den Typ Set definiert.
//type Set = Int => Boolean
//
//Implementieren Sie die folgenden Funktionen:
//a) Eine Funktion createEmptySet:Set, die eine leere Menge erzeugt.
//b) Eine Funktion contains(elem:Int, set:Set):Boolean, die ermittelt, ob ein Element
//in der Menge ist oder nicht.
//c) Eine Funktion insert(elem:Int, set:Set):Set, die ein Element in das Set einfügt.
//d) Eine Funktion createRange(a:Int,b:Int):Set, die alle Elemente von a bis b in die
//Liste schreibt.
//e) Eine Funktion union(set1:Set, set2:Set):Set, die zwei Mengen vereinigt.
//f) Eine Funktion toList(set:Set,a:Int,b:Int):List[Int], die alle Elemente in dem
//Bereich [a,...,b], die in der Menge sind in eine Liste.
//g) Entwickeln Sie Tests für Ihre Mengenfunktionen.

// ähnlich zu lambda functions
// anonymous high order functions
// man nutzt inheritance, skippt OOP

type Set = Int => Boolean //Set is any function that takes an Int and returns a Boolean

def createEmptySet: Set =
  x => false // erwartungswert: Int => Boolean (Set)

def contains(i: Int, s: Set): Boolean =
  s(i) // int zu boolean, d.h. set(int) = boolean, erwartungswert: Boolean

//insert returns a new function (Int => Boolean).
//This function takes any integer x and says:
//  true if x is the new element (x == i), or
//  true if x was already in the original set (s(x)).
def insert(i: Int, s: Set): Set =
  x => x==i || s(x) //man kann auch contains(x,s) schreiben

def createRange(a: Int, b: Int): Set = {
  x => x>=a && x<=b
  // return: Int => boolean, same as x => true && true
}

def union(set1: Set, set2: Set): Set =
  x => contains(x, set1) || contains(x, set2)

//list(1,2,3) == 1::2::3::list(), cons-operator(head::tail)
//Aufzählungsliste: a::toList()
def toList(set:Set, a:Int, b:Int):List[Int] = {
  if (a>b) List() // base case: stop when range is done
  else if (contains(a, set)) // check if current element is in set
    a :: toList(set, a+1, b) // include it and recurse
  else toList(set, a+1, b) // skip it and recurse
}


def set1 = createEmptySet
val set2 = insert(4, set1)

