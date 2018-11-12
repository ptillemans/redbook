package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.concurrent.duration.TimeUnit


object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean):Boolean = false
  }


  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]) : Future[A] =
    a(s)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  case class Map2Future[A,B,C](a: Future[A],
                               b: Future[B],
                               f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get(timeout: Long, units: TimeUnit) =
      compute(TimeUnit.NANOSECONDS.convert(timout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => C
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanon - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }


  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
                      def call = a(es).get
                    })

  def lazyUnit[A](a: A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

}
