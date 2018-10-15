package chapter5

import org.scalatest.FlatSpec
import chapter5.Stream

class StreamTest extends FlatSpec {

  "toList" should "turn stream into list" in {
    val s: Stream[Int] = Stream(1, 2, 3)
    val es: List[Int] = List(1, 2, 3)

    assert(s.toList == es)
  }

  "take" should "for 3 return first 3 items in stream" in {
    val s = Stream(1, 2, 3, 4, 5)
    val es = List(1, 2, 3)
    assert(s.take(3).toList == es)
  }

  it should "for 0 return empty stream" in {
    val s = Stream(1, 2, 3, 4, 5)

    assert(s.take(0) == Stream.empty)
  }

  it should "for 3 from empty stream returns empty stream" in {
    val s:Stream[Int] = Stream.empty
    assert(s.take(3) == Stream.empty)
  }

  "takeWhile" should "with even predicate returns first even elements" in {
    val isEven: Int => Boolean = _ % 2 == 0
    val s = Stream(2, 4, 6,7,8)
    val e = List(2, 4, 6)

    assert(s.takeWhile(isEven).toList == e)
  }

  "forAll" should "return true when all elements satisfy predicate" in {
    val isEven: Int => Boolean = _ % 2 == 0
    val s = Stream(2, 4, 6, 8, 10)

    assert(s.forAll(isEven))
  }

  it should "return false when any element fails the predicate" in {
    val isEven: Int => Boolean = _ % 2 == 0
    val s = Stream(2, 4, 5, 8, 10)

    assert(s.forAll(isEven) == false)
  }

  "headOption" should "return just first value if non empty stream" in {
    val s = Stream(1, 2, 3, 4, 5)

    assert(s.headOption() == Some(1))
  }

  it should "return Nothing if stream empty" in {
    val s: Stream[Int] = Stream.empty

    assert(s.headOption() == None)
  }

  "map" should "apply a function to all elements" in {
    val s = Stream(1, 2, 3)
    val e = List(11, 12, 13)

    assert(s.map(_ + 10).toList == e)
  }

  "filter" should "only return elements satisfying the predicate" in {
    val s = Stream(1, 2, 3, 4, 5)
    val e = List(2, 4)
    val p: (Int => Boolean) = _ % 2 == 0

    assert(s.filter(p).toList == e)
  }

  "append" should "concatenate 2 streams" in {
    val a = Stream(1, 2, 3)
    val b = Stream(4, 5)
    val e = List(1, 2, 3, 4, 5)

    assert(a.append(b).toList == e)
  }

  it should "return the stream when appended to an empty stream" in {
    val a: Stream[Int] = Stream.empty
    val b = Stream(1, 2, 3)

    assert(a.append(b).toList == b.toList)
  }

  "flatMap" should "unwrap a wrapped stream" in {
    val a = Stream(1, 2, 3)
    val f: Int => Stream[Int] = x => Stream.cons(x, Empty)

    assert(a.flatMap(f).toList == List(1, 2, 3))
  }

  "contant" should "return infinite stream of constant value" in {
    val s = Stream.constant(42)

    assert(s.take(5).toList == List(42, 42, 42, 42, 42))
  }

  "from" should "return a stream of incrementing integers" in {
    val s = Stream.from(7)

    assert(s.take(5).toList == List(7, 8, 9, 10, 11))
  }

  "fibs" should "return the fibonacci sequence" in {
    val s = Stream.fibs()

    assert(s.take(7).toList == List(1, 1, 2, 3, 5, 8, 13))
  }

  "unfold" should "return values till function returns None" in {
    val f: Int => Option[(Int, Int)] =
      x => if (x < 5) Some((x, x + 1))
           else None

    assert(Stream.unfold(0)(f).toList == List(0, 1, 2, 3, 4))
  }


  "zipWith" should "zip two streams to the shortest length" in {
    val a = Stream("foo", "bar", "baz")
    val b = Stream(1, 2, 3, 4, 5)
    val e = List(("foo", 1), ("bar", 2), ("baz", 3))

    assert(a.zipWith(b).toList == e)
  }

  "zipAll" should "zip all elements of both strings to options" in {
    val a = Stream("foo", "bar")
    val b = Stream(1, 2, 3)
    val e = List((Some("foo"), Some(1)),
                 (Some("bar"), Some(2)),
                 (None, Some(3)))

    assert(a.zipAll(b).toList == e)
  }

  "startsWith" should "return true when a starts with b" in {
    val a = Stream(1, 2, 3)
    val b = Stream(1, 2)

    assert(a.startsWith(b))
  }

  it should "return false when a does not start with b" in {
    val a = Stream(1, 2, 3)
    val b = Stream(2,3)

    assert(!a.startsWith(b))
  }

  it should "return false when b longer than a" in {
    val a = Stream(1, 2)
    val b = Stream(1, 2, 3)

    assert(!a.startsWith(b))
  }

  "tails" should "return subsequent tails of the stream" in {
    val a = Stream(1, 2, 3)
    val e = List(List(1, 2, 3), List(2, 3), List(3))

    assert(a.tails().map(_.toList).toList == e)
  }

  "hasSubsequence" should "return true if b is subsequence of a" in {
    val a = Stream(1, 2, 3, 4, 5)
    val b = Stream(2, 3, 4)

    assert(a.hasSubsequence(b))
  }

  it should "return false when b is no subsequence" in {
    val a = Stream(1, 2, 3, 4, 5)
    val b = Stream(2, 4, 5)

    assert(!a.hasSubsequence(b))
  }

  "scanRight" should "return aggregates of the tails" in {
    val a = Stream(1, 2, 3)
    val e = List(6, 5, 3, 0)

    assert(a.scanRight(0)(_ + _).toList == e)
  }


}
