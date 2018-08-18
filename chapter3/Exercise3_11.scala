
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]


object List {

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x,foldRight(xs,z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def sum(ints: List[Int]): Int = foldLeft(ints,0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds,1.0)(_ * _)

  def count[A](as: List[A]): Int = foldLeft(as,0)((z, _) => z + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as,Nil:List[A])((z, a) => Cons(a, z))

  def forEach[A](as: List[A])(f:(A => Unit)): Unit = foldLeft(as, ())((_, a) => f(a) )


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

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else as
    }

  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
}

object Exercise3_6 {

  def main(args: Array[String]): Unit = {
    val x = List(1,2,3)
    println("x = %d".format(List.sum(x)))

    val y = List.tail(x)

    println("y = %d".format(List.sum(y)))

    val z = List.setHead(x, 3)
    println("z = %d".format(List.sum(z)))

    val u = List.drop(x,2)
    println("u = %d".format(List.sum(u)))

    val v = List.dropWhile(x)(x => x % 2 == 1)
    println("v = %d".format(List.sum(v)))

    val w = List.init(x)
    println("w = %d".format(List.sum(w)))

    val c = List.count(x)
    println("count = %d".format(c))

    val r = List.reverse(x)
    println("count(r) = %d".format(List.count(r)))
    List.forEach(r)(x => print(" %s".format(x)))

  }


}
