package Chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B) : B =
    t match {
      case Leaf(x) => f(x)
      case Node(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def size[A](t: Tree[A]) : Int =
    fold(t)(_ => 1)(_ + _)

  // Exercise 3.26
  def maximum(t: Tree[Int]) : Int =
    fold(t)(x => x)(_.max(_))

  // Exercise 3.27
  def depth[A](t: Tree[A]) : Int =
    fold(t)(_ => 1)(1 + _.max(_))

  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] =
    fold(t)(a => Leaf(f(a)):Tree[B])(Node(_,_):Tree[B])

}
