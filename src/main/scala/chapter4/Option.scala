package chapter4

import chapter3.{List, Cons, Nil}
import scala.{Option => _, Either => _, _}


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B >: A](default: B): B =
    this match {
      case None => default
      case Some(b) => b
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: Option[B]): Option[B] =
    this map(Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  def map2[A,B, C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(x => b.map(f(x,_)))

  def sequence[A](oas: List[Option[A]]) : Option[List[A]] =
    List.foldRight(oas, Some(Nil:List[A]):Option[List[A]])(
      (oa, obs) => oa.flatMap(a => obs.map(Cons(a, _))))

}
