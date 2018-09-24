
object Exercise2_1 {


  @annotation.tailrec
  def fib(n: Int,a: Int = 0, b:Int = 1): Int = {
    if (n == 0)
      a
    else if (n == 1)
      b
    else
      fib(n-1, b, a+b)
  }

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 10)
      println(fib(i))
  }

}
