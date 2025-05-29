import SheetUtils.printAnswer

import scala.annotation.tailrec

object Fun {

  //decidi saltar os exs do 1 porque são "passos" para começar a ficha

  //Ex2a
  def ex2a(p1:(Int,Int), p2:(Int,Int)):(Int,Int) = (p1._1+p1._2,p2._1*p2._2)

  //Ex2b
  //método que devolve o maior de dois valores
  def higher(a: Int, b: Int): Int = if (a > b) a else b

  //método que devolve o menor de dois valores
  def lower(a:Int, b:Int):Int = if(a < b) a else b

  //método que devolve o maior valor de 3 valores
  def higher3(a: Int, b: Int, c: Int): Int = higher(higher(a, b), c)

  //método que devolve o segundo maior valor de 3 valores (ou seja, o valor do meio)
  def middle3(a:Int, b:Int, c:Int):Int = lower(higher(a,b), higher(lower(a,b),c))

  def ex2b(a:Int, b:Int, c:Int): (Int, Int) = (higher3(a,b,c), middle3(a,b,c))

  //Ex2c
  //método que devolve o menor de 3 valores
  def lower3(a:Int, b:Int, c:Int): Int = lower(lower(a,b),c)

  def ex2c(a:Int, b:Int, c:Int): (Int,Int,Int) = (higher3(a,b,c), middle3(a,b,c), lower3(a,b,c))

  //Ex2d
  def ex2d(s1:Int, s2:Int, s3:Int): Boolean = s1 + s2 > s3 && s1 + s3 > s2 && s2 + s3 > s1

  //Ex2e
  def abrev(name:String):String = name.split(" ").head + " " + name.split(" ").last

  //Ex3a
  def pow(x:Int, y:Int):Int = if(y==0) 1 else x * pow(x,y-1)

  //Ex3b
  def firstAndLast(lst:List[Int]): (Int,Int) = (lst.head, lst.last)

  //Ex3c
  def lstWithLen(lst: List[Int]): (List[Int], Int) = (lst, lst.length)

  //Ex3d
  def sum(lst: List[Double], acc:Double=0):Double = if (lst.isEmpty) acc else sum(lst.tail, lst.head + acc)

  def mean(lst: List[Double]):Double = sum(lst)/lst.length

  //método main para testar os métodos dos exs
  def main(args: Array[String]): Unit = {
    printAnswer("2a", ex2a((1,2),(2,3)).toString())
    printAnswer("2b", ex2b(6,1,3).toString())
    printAnswer("2c", ex2c(7,6,9).toString())
    printAnswer("2d", ex2d(5,3,4).toString)
    printAnswer("2e", abrev("Diogo André Rosa Lourenço"))
    printAnswer("3a", pow(2,7).toString)
    printAnswer("3b", firstAndLast(List(4,7,2,56,89,0,1)).toString())
    printAnswer("3c", lstWithLen(List(1,2,3,4,5,6,7)).toString())
    printAnswer("3d", mean(List(10,13,15,18)).toString)
  }
}
