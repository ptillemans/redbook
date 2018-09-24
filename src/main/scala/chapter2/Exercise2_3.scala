
object Excercise2_3 {

  def curry[A,B,C](f:(A, B) => C): A => (B => C) =
    a => (b => f(a,b))

  def sum(a:Int, b:Int) : Int =
    a + b

  def main(args:Array[String]): Unit = {
    val sum3 = curry(sum)(3)
    println(sum3(10))
  }

}
