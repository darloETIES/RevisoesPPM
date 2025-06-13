import SheetUtils.printAnswer

import scala.annotation.tailrec

object Sheet3 {

  //Ex1.1a - sem if
  def fact1(a:Int):Int = a match {
    case 0 => 1
    case x => x * fact1(x-1)
  }

  //Ex1.1b - com if
  def fact2(a:Int):Int = if(a==0) 1 else a * fact2(a-1)

  //Ex1.1c - tail recursion
  @tailrec
  def fact3(a:Int, acc:Int=1):Int = a match {
    case 0 => acc
    case x => fact3(x-1, x*acc)
  }

  //outra forma seria com metodo aninhado (assim não teriamos necessidade de ter o acc como parametro da assinatura principal)
  def fact3v2(a:Int):Int = {
    @tailrec
    def fact(a:Int, acc:Int):Int = a match {
      case 0 => acc
      case x => fact(x-1, x*acc)
    }
    fact(a,1)
  }

  //Ex1.2a
  def remDup1[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case x::xs => x :: remDup1(xs.dropWhile(_==x))
  }

  //Ex1.2b
  def remDupTR[A](lst:List[A]):List[A] = {
    @tailrec
    def aux[A](l:List[A], lAcum:List[A]):List[A] = l match {
      case Nil => lAcum
      case x::xs => aux(xs.dropWhile(_==x), lAcum:+x) //aparece primeiro a lista acumulada porque senão a lista ficaria ordenada ao contrario (fazer manualmente para entender)
      //se fosse "... , x::lAcum)"
      //aux( List(1,1,2),Nil)
      //  aux(List(2), 1::Nil)
      //    aux(Nil, 2::1::Nil)
    }
    aux(lst,Nil)
  }

  //Ex3a
  def addElemLists(l1:List[Int], l2:List[Int]):List[Int] = (l1,l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x1::xs1, x2::xs2) => x1+x2 :: addElemLists(xs1, xs2)
  }

  //Ex3b
  def zipWith[A,B,C](l1:List[A], l2:List[B], f:(A,B)=>C):List[C] = (l1,l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x1::xs1, x2::xs2) => f(x1,x2) :: zipWith(xs1,xs2, f)
  }

  def sum(a:Int, b:Int):Int = a+b
  def mult(a:Int, b:Int):Int = a*b
  def div(a:Int, b:Int):Double = a.toDouble/b.toDouble
  def concatChar(a:String, b:String):String = a+b

  //Ex3c
  def isSorted[A](lst: List[A], ordered:(A,A) => Boolean):Boolean = lst match {
    case Nil => true
    case List(x) => true
    case x::xs => ordered(x,xs.head) && isSorted(xs, ordered)
  }

  def asc(a:Int, b:Int):Boolean = a<b // menor -> maior
  def desc(a:Int, b:Int):Boolean = a>b //maior -> menor

  //Ex3d
  @tailrec
  def bubbleSort(data:List[Int], f:(Int,Int)=>Boolean):List[Int] = {
    def loop(d:List[Int]):List[Int] = {
      d match {
        case Nil => Nil
        case List(x) => List(x)
        case x::y::xs => if(f(x,y)){
          x::loop(y::xs)
        } else{
          y::loop(x::xs)
        }
      }
    }
    if(isSorted(data, f)) data
    else bubbleSort(loop(data), f)
  }


  def main(args: Array[String]): Unit = {
    printAnswer("1.1a", fact1(5).toString)
    printAnswer("1.1b", fact2(5).toString)
    printAnswer("1.1c", fact3(5).toString)
    printAnswer("1.1c V2", fact3v2(5).toString)

    printAnswer("1.2a", remDup1(List('a','a','a','b','c','c','c')).toString())
    printAnswer("1.2b", remDupTR(List('a','a','a','b','c','c','c')).toString())

    printAnswer("3a", addElemLists(List(1,2,3), List(4,5,6)).toString())
    printAnswer("3b (sum)", zipWith(List(1,2,3), List(4,5,6), sum).toString())
    printAnswer("3b (div)", zipWith(List(1,2,3), List(4,5,6), div).toString())
    printAnswer("3b (concatChar)", zipWith(List("a","b","a"), List("b","c","d"), concatChar).toString())
    printAnswer("3c" , isSorted(List(3,2,1,-1), desc).toString)
    printAnswer("3d", bubbleSort(List(4,0,3,5,1,2,8), asc).toString())
  }
}
