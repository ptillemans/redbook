package chapter4

import chapter3.{List, Cons, Nil}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def orElse[EE>:E, B>:A](b: Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    b.flatMap(b => this.map(f(_,b)))

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def  traverse[E, A, B](es: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    List.foldRight(es, Right(Nil:List[B]):Either[E,List[B]])(
      (a, ebs) => ebs.flatMap(bs => f(a).map(Cons(_,bs))))

}
