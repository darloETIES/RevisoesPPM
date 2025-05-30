import SheetUtils.printAnswer

import scala.::
import scala.annotation.tailrec

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

  //Ex1e
  def sumEl(lst:List[(Int,Int)]):Int = {
    def aux(l:List[(Int, Int)], c:Int, acc:Int): Int = {
      l match {
        case Nil => acc
        case x::xs =>
          if(c==2 || c==4) aux(xs, c+1, acc + x._1 + x._2)
          else aux(xs, c+1, acc)
      }
    }
    aux(lst,0,0)
  }

  //Ex1fi
  def lenAndSum(lst:List[Double]):(Int,Double) = {
    def aux(l:List[Double], c:Int, acc:Double):(Int, Double) = {
      l match {
        case Nil => (c,acc)
        case x::xs => aux(xs, c+1, acc+x)
      }
    }
    aux(lst, 0, 0)
  }

  //Ex1fii
  def avgUpdated(lst:List[Double]):Double = {
    val (nElem, sumElem) = lenAndSum(lst)
    sumElem/nElem
  }

  //Ex1g
  def metH(lst:List[Double], v:Double):(List[Double], List[Double]) = {
    lst match {
      case Nil => (Nil, Nil)
      case x::xs =>
        if(x < v) (x::metH(xs,v)._1, metH(xs,v)._2)
        else (metH(xs,v)._1, x::metH(xs,v)._2)
    }
  }


  //Dados do Ex2:
  type Entry = (String, String, String) //(name, phone, email)
  type LTelef = List[Entry]

  def emails(lst:LTelef) : List[String] = {
    lst match {
      case Nil => Nil
      case (_, _, email) :: tail => email :: emails(tail)
    }
  }

  //Ex2i
  def emailsWithFixedPhonePrefix(lst: LTelef):List[String] = {
    lst match {
      case Nil => Nil
      case (_,phone,email)::xs =>
        if(phone.head == '2') email::emailsWithFixedPhonePrefix(xs)
        else emailsWithFixedPhonePrefix(xs)
    }

  }

  //Ex2j
  def emailAndPhoneFromName (lst:LTelef, name:String):(String, String) = {
    lst match {
      case Nil => ("","")
      case (n,phone,email)::xs =>
        if(name.equals(n)) (phone,email)
        else emailAndPhoneFromName(xs, name)
    }
  }

  //ExEXTRA
  def divide[A](lst:List[A]):(List[A], List[A]) = {
    def aux[A](l:List[A], c:Int): (List[A], List[A]) = {
      l match {
        case Nil => (Nil, Nil)
        case x::xs =>
          if(c < lst.length/2) (x::aux(xs, c+1)._1, aux(xs, c+1)._2)
          else (aux(xs, c+1)._1, x::aux(xs, c+1)._2)
      }
    }
    aux(lst, 0)
  }

  def main(args: Array[String]): Unit = {
    printAnswer("1a", transf(List(1,2,3,4,5,6)).toString())
    printAnswer("1b", prod(List(2,4,8)).toString)
    printAnswer("1c", placeLast(List(1,2,3,4),5).toString())
    printAnswer("1d", concatLists(List(1,2,3,4), List(5,6,7,8,9)).toString())
    printAnswer("1e", sumEl(List((1,2),(3,4),(2,0),(5,6),(1,1))).toString)
    printAnswer("1f i", lenAndSum(List(10,11.5,16.7,13.6)).toString())
    printAnswer("1f ii", avgUpdated(List(10,11.5,16.7,13.6)).toString)
    printAnswer("1g", metH(List(1.0,2.0,4.0,5.0), 3.0).toString())

    //Dados de teste para o ex2
    val lst:LTelef = List(
      ("Di Goat", "215765493", "digoat@amassando.pt"),
      ("Ze Enrabado", "945763195", "itszeover@notstonks.com"),
      ("Tiago Ter Mentalidade Campeão Taver", "219763486", "sco.ser.vida@touafalardacadeira.pt"),
      ("Serjao Qta das Pegas Boss", "935692381", "qtadaspegas.4ever@taver.pt")
    )

    printAnswer("2i", emailsWithFixedPhonePrefix(lst).toString())
    printAnswer("2j", emailAndPhoneFromName(lst, "Ze Enrabado").toString())
    printAnswer("EXTRA", divide[String](List("a","b","c","d","e","f","g","h")).toString())
  }
}
