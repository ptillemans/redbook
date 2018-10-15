package chapter5


sealed trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

  // def take(n: Int): Stream[A] =
  //   if (n > 0)
  //     this match {
  //       case Empty => Empty
  //       case Cons(h, t) => Cons(h, () => t().take(n - 1))
  //     }
  //   else Empty

  // def takeWhile(p: A => Boolean): Stream[A] =
  //   this match {
  //     case Empty => Empty
  //     case Cons(h, t) =>
  //       if (p(h())) Cons(h, () => t().takeWhile(p))
  //       else Empty
  //   }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // def takeWhile(p: A => Boolean): Stream[A] =
  //   foldRight(Empty:Stream[A])((a, b) =>
  //     if (p(a))
  //       Cons(() => a, () => b)
  //     else
  //       Empty
  //   )

  def headOption(): Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // def map[B](f: A => B): Stream[B] =
  //   foldRight(Empty:Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a, b) =>
      if (p(a))
        Cons(() => a, () => b)
      else
        b
    )

  def append[B >: A](as: => Stream[B]): Stream[B] =
    foldRight(as)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this)(
      as => as match {
        case Empty => None
        case Cons(h, t) => Some(f(h()), t())
      })

  def take(n: Int): Stream[A] =
    Stream.unfold((n,this))(
      (t) => t match {
        case (n: Int, as: Stream[A]) => as match {
          case Empty => None
          case Cons(h, t) =>
            if (n > 0) Some(h(), (n - 1, t()))
            else None
        }
      })

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(
      as => as match {
        case Empty => None
        case Cons(h, t) => {
          val a = h()
          if (p(a)) Some(a, t())
          else None
        }
      })

  def zipWith[B](bs: Stream[B]): Stream[(A,B)] =
    Stream.unfold((this,bs))(
      (t) => t match {
        case(as, bs) => as match {
          case Empty => None
          case Cons(ah, at) => bs match {
            case Empty => None
            case Cons(bh, bt) => Some((ah(), bh()), (at(), bt()))
          }
        }
      })

  def tail(): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) => t()
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, bs))(
      (t) => t match {
        case(as, bs) => {
          val oa = as.headOption()
          val ob = bs.headOption()
          if (oa.isEmpty && ob.isEmpty) None
          else Some((oa, ob), (as.tail(), bs.tail()))
        }
      })

  def startsWith[B >: A](bs: Stream[B]): Boolean =
    !this.zipAll(bs)
      .filter(t => !t._2.isEmpty)
      .exists(t => t._1 != t._2)

  def tails(): Stream[Stream[A]] =
    Stream.unfold(this)(
      s => s match {
        case Cons(_,_) => Some(s, s.tail())
        case Empty => None
      })

  def hasSubsequence[B >: A](bs : Stream[B]): Boolean =
    tails exists (_ startsWith bs)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight(Stream.cons(z,Stream.empty))(
      (a, bs) => bs match {
        case Cons(b, _) => Stream.cons(f(a, b()), bs)
      })
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] =
    return Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // def constant[A](a: A): Stream[A] =
  //   Stream.cons(a, constant(a))

  // def from(n: Int): Stream[Int] =
  //   Stream.cons(n, from(n+1))

  // def iterate[A](a: A)(f: A => A): Stream[A] =
  //   Stream.cons(a, iterate(f(a))(f))

  // def fibs(): Stream[Int] =
  //   iterate((1,1))((t) => (t._2, t._1 + t._2)).map((t) => t._1)

  def unfold[A, S](s: S)(f: S => Option[(A,S)]): Stream[A] =
    f(s) match {
      case Some(t) => Stream.cons(t._1, unfold(t._2)(f))
      case None => Stream.empty
    }

  def constant[A](a: A): Stream[A] =
    unfold(a)(a => Some((a, a)))

  def from(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def iterate[A](a: A)(f: A => A): Stream[A] =
    unfold(a)(x => {
                       val y = f(x)
                       Some(y, y)
                     })

  def fibs(): Stream[Int] =
    unfold((1,1))((t) => Some(t._1, (t._2, t._1 + t._2)))



}
