import SheetUtils.printAnswer

object Sheet2 {

  //Ex1a
  def transf[A](lst:List[A]):List[A] =
    lst match {
      case Nil => Nil
      case x::Nil => List(x) //caso para um elemento na lista, de modo a evitar o erro de quando a lista for de tamanho ímpar (sobrará um para comparar, comparando com Nothing)
      case x::y::xs => y::x::transf(xs)
    }

  //Ex1b
  def prod(lst:List[Int]):Int = {
    lst match {
      case Nil => 1 //de modo a multiplicar por 1 no final, mantendo o mesmo resultado
      case x::xs => x * prod(xs)
    }
  }

  //Ex1c
  def placeLast[A](lst:List[A], elem:A):List[A] = lst :+ elem

  //Ex1d
  def concatLists[A](l1:List[A], l2:List[A]):List[A] = {
    l1 match {
      case Nil => l2
      case x::Nil => x :: l2
      case x::xs => x :: concatLists(xs, l2)
    }
  }

  def main(args: Array[String]): Unit = {
    printAnswer("1a", transf(List(1,2,3,4,5,6)).toString())
    printAnswer("1b", prod(List(2,4,8)).toString)
    printAnswer("1c", placeLast(List(1,2,3,4),5).toString())
    printAnswer("1d", concatLists(List(1,2,3,4), List(5,6,7,8,9)).toString())
  }
}
