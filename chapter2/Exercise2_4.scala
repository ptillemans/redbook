
object Exercise2_4 {

  def curry[A,B,C](f:(A, B) => C): A => (B => C) =
    a => (b => f(a,b))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def sum(a:Int, b:Int): Int =
    a + b

  def main(args: Array[String]): Unit = {
    val curried_sum = curry(sum)
    val sum3 = curried_sum(3)
    val uncurried_sum = uncurry(curried_sum)

    println(curried_sum(3)(5))
    println(sum3(5))
    println(uncurried_sum(3, 5))
  }
}
