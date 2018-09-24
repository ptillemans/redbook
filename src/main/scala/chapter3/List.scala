sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]


object List {

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x,foldRight(xs,z)(f))
  }


  // Excercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }


  def forEach[A](as: List[A])(f:(A => Unit)): Unit = foldLeft(as, ())((_, a) => f(a) )


  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](as: List[A], a:A): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(a, xs)
  }

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, xs) => if (n > 1) drop(xs, n - 1) else xs
    }

  // Exercise 3.5
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else as
    }

  // Exercise 3.6
  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldLeft(as,0)((z, _) => z + 1)

  // Exercise 3.10 : see above

  // Exercise 3.11
  def sum(ints: List[Int]): Int = foldLeft(ints,0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds,1.0)(_ * _)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as,Nil:List[A])((z, a) => Cons(a, z))

  // TODO: Exercise 3.13

  // Excercise 3.14
  def append[A](as: List[A], bs: List[A] ): List[A] =
    foldRight(as, bs)(Cons(_, _))

  // Excercise 3.15
  def flatten[A](ass: List[List[A]]) : List[A] =
    foldRight(ass, Nil:List[A])(append)

  // Exercise 3.16
  def increment(as: List[Int]) : List[Int] =
    foldLeft(as,Nil:List[Int])((bs, x) => (Cons(x + 1, bs)))

  // Exercise 3.17
  def doublesToString(ds: List[Double]) : String =
    foldLeft(ds, "")((d, s) => s + d.toString())

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A => B) : List[B] =
    foldLeft(as, Nil:List[B])((bs, a) => Cons(f(a), bs))

}

object Chapter3 {

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

    val c = List.length(x)
    println("length = %d".format(c))

    val r = List.reverse(x)
    println("length(r) = %d".format(List.length(r)))
    List.forEach(r)(x => print(" %s".format(x)))
    println()

    val app = List.append(x, y)
    println("append(x, y)")
    List.forEach(app)(x => print(" %s".format(x)))
    println()

    val con = List.flatten(List(x, y, z))
    println("flatten(x, y, z)")
    List.forEach(con)(x => print(" %s".format(x)))
    println()

    val inc = List.increment(x)
    println("inc(x)")
    List.forEach(inc)(x => print(" %s".format(x)))
    println()

    val ds = List(1.0, 2.0, 3.0)
    println("doublesToString(ds)")
    println(List.doublesToString(ds))
    println()

    val map = List.map(x)(1 + _)
    println("map(x)")
    List.forEach(map)(x => print(" %s".format(x)))
    println()
  }


}
