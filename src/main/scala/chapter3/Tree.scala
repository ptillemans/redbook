package Chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]) : Int =
    t match {
      case Leaf(_) => 1
      case Node(left, right) => length(left) + length(right)
    }

  // Exercise 3.26
  def maximum(t: Tree[Int]) : Int =
    t match {
      case Leaf(x) => x
      case Node(left, right) => maximum(left).max(maximum(right))
    }

  // Exercise 3.27
  def depth[A](t: Tree[A]) : Int =
    t match {
      case Leaf(_) => 1
      case Node(left, right) => 1 + depth(left).max(depth(right))
    }

  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Node(left, right) => Node(map(left)(f), map(right)(f))
    }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: (A,B) => B)(g: (B, B) => B) : B =
    t match {
      case Leaf(x) => f(x)
      case Node(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
}
