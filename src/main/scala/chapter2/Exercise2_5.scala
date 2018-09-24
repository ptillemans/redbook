object Exercise2_5 {

  def compose[A,B,C](f: A => B, g: B => C): A => C =
    a => g(f(a))


  def add3(x:Int) = x + 3
  def double(x:Int) = 2 * x

  def main(args:Array[String]) {
    val add3ToDouble = compose(double, add3)

    println(add3ToDouble(5))
  }
}
