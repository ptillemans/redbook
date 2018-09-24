// A comment
/* another comment */
/** a documentation comment **/
object MyModule {

  def abs(n: Int): Int =
    if (n < 0)  -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int = 1): Int = {
      if (n <= 1)
        acc
      else
        go(n - 1, n * acc)
    }
    go(n)
  }
  private def formatAbs(n : Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(n, abs(n))
  }

  private def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }

}
