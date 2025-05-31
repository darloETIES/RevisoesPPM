import SheetUtils.printAnswer

object PPT3Exercices {

  //Higher Order Functions
  //1.
  def product(f:Int => Int, a:Int, b:Int):Int = {
    if(a>b) 1
    else f(a) * product(f, a+1, b)
  }

  //2.
  def factorial(n:Int):Int = product(x => x, 1, n)

  //3.
  def operator(f:(Int,Int) => Int, a:Int, b:Int):Int = {
    if(a>=b) b //pois será o último valor no intervalo
    else f(a, operator(f, a+1, b))
  }

  //Mapping
  //sem map
  def squareList(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case y::ys => y*y :: squareList(ys)
    }
  }

  //com map
  def squareListMap(xs: List[Int]): List[Int] = {
    xs map (x => x*x)
  }

  def prod(a:Int, b:Int):Int = operator((x,y) => x*y, a,b)
  def sum(a:Int, b:Int):Int = operator((x,y) => x+y, a, b)

  def main(args: Array[String]): Unit = {
    printAnswer("1 - Higher Order Functions", product(x => x, 3,6).toString)
    printAnswer("2 - Higher Order Functions", factorial(5).toString)
    printAnswer("3 - Higher Order Function", "Product: " + prod(3,6) + "\nSum: " + sum(3,6))
    printAnswer("Mapping", "Without map: " + squareList(List(2,4,8)) + "\nWith map: " + squareListMap(List(2,4,8)))
  }

}
