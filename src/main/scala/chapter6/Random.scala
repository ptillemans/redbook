package chapter6

import chapter6.State

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}


object Random {

  type Rand[A] = State[RNG, A]

  def int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (n, nextRNG) = rng.nextInt
    if (n < 0)
      (n - Int.MinValue, nextRNG)
    else
      (n, nextRNG)
  })

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)


  // def double(rng: RNG) : (Double, RNG) = {
  //   val (n, nextRng) = nonNegativeInt(rng)
  //   (n.toDouble/Int.MaxValue.toDouble, nextRng)
  // }

  // def doubleInt: Rand[(Double, Int)] = {
  //   val (d, rng1) = double(rng)
  //   val (n, rng2) = integer(rng1)
  //   return ((d, n), rng2)
  // }

  // def intDouble(rng: RNG) : ((Int, Double), RNG) = {
  //   val (n, rng1) = integer(rng)
  //   val (d, rng2) = double(rng1)
  //   return ((n, d), rng2)
  // }

  // def double3(rng: RNG) : ((Double, Double, Double), RNG) = {
  //   val (d1, rng1) = double(rng)
  //   val (d2, rng2) = double(rng)
  //   val (d3, rng3) = double(rng)
  //   return ((d1, d2, d3), rng3)
  // }



  // def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  //   rng => {
  //     val (a, rng1) = s(rng)
  //     (f(a), rng1)
  //   }

  // def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //   rng => {
  //     val (a, rng1) = ra(rng)
  //     val (b, rng2) = rb(rng1)
  //     (f(a,b), rng2)
  //   }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(State.unit[RNG,List[A]](List()))(map2(_,_)((a: A, as:List[A]) => a :: as))

  def flatMap[A, B](f:Rand[A])(g: A => Rand[B]): Rand[B] =
    State(rng => f.run(rng) match {
      case (a, rng2) => g(a).run(rng2)
    })

  def map[A, B](f:Rand[A])(g: A => B): Rand[B] =
    flatMap(f)(a => State.unit(g(a)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + n - 1 - mod >= 0) State.unit(mod)
      else nonNegativeLessThan(n)
    }

}
