
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](as: List[A], a:A): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(a, xs)
  }

  def drop[A](as: List[A], n: Int): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, xs) => if (n > 1) drop(xs, n - 1) else xs
    }

}

object Exercise3_4 {

  def main(args: Array[String]): Unit = {
    val x = List(1,2,3)
    println("x = %d".format(List.sum(x)))

    val y = List.tail(x)

    println("y = %d".format(List.sum(y)))

    val z = List.setHead(x, 3)
    println("z = %d".format(List.sum(z)))

    val u = List.drop(x,2)
    println("u = %d".format(List.sum(u)))

  }


}
