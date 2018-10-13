package chapter4

import org.scalatest.FlatSpec
import chapter4.Either
import chapter3.List

class EitherTest extends FlatSpec {

  "Either.map" should "map right values" in {
    val e: Either[String, Int] = Right(7)
    e.map((x:Int) => x * 6) match {
      case Right(x) => assert(x == 42)
      case _ => fail("expected Right(42)")
    }
  }

  it should "pass on left values" in {
    val e: Either[String, Int] = Left("Foo")
    e.map((x:Int) => x * 6) match {
      case Left(s) => assert(s == "Foo")
      case _ => fail("expected Left(\"Foo\")")
    }
  }

  "Either.orElse" should "return right values" in {
    val e: Either[String, Int] = Right(7)
    e.orElse(Right(42)) match {
      case Right(x) => assert(x == 7)
      case _ => fail("expected Right(7)")
    }
  }

  it should "return default value when left value" in {
    val a: Either[String, Int] = Left("foo")
    val b: Either[String, Int] = Right(42)
    a.orElse(b) match {
      case Right(x) => assert(x == 42)
      case _ => fail("expected default value")
    }
  }

  "Either.flatMap" should "unwrap the right value" in {
    val a: Either[String, Int] = Right(7)

    a.flatMap(x => Right(x * 6)) match {
      case Right(x) => assert(x == 42)
      case _ => fail("expected unwrapped value in either")
    }
  }

  it should "pass left values along" in {
    val a: Either[String, Int] = Left("foo")
    a.flatMap(x => Right(x * 6)) match {
      case Left(s) => assert(s == "foo")
      case _ => fail("expected left value to be returned")
    }
  }

  "Either.map2" should "map the right values" in {
    val a: Either[String, Int] = Right(7)
    val b: Either[String, Int] = Right(6)

    a.map2(b)(_ * _) match {
      case Right(x) => assert(x == 42)
      case _ => fail("expected left value")
    }
  }

  it should "return any left values" in {
    val a: Either[String, Int] = Left("foo")
    val b: Either[String, Int] = Right(7)

    a.map2(b)(_ * _) match {
      case Left(s) => assert(s == "foo")
      case _ => fail("expected left value")
    }

    b.map2(a)(_ * _) match {
      case Left(s) => assert(s == "foo")
      case _ => fail("expected left value")
    }
  }

  "Either.Try" should "map success to right values" in {
    Either.Try("42".toInt) match {
      case Right(x) => assert(x == 42)
      case _ => fail("expected left value")
    }
  }

  it should "return failure in left values" in {
    Either.Try("foo".toInt) match {
      case Left(e: Exception) => assert(e.getMessage() == "For input string: \"foo\"")
      case _ => fail("expected left value")
    }
  }

  "Either.traverse" should "map success to right with list" in {
    val as: List[String] = List("6", "7")
    val expected: List[Int] = List(6, 7)
    val f: String => Either[Exception, Int] =
      s => Either.Try(s.toInt)

    Either.traverse(as)(f) match {
      case Right(es) => assert( es == expected )
      case _ => fail("expected left value")
    }
  }

  it should "return first error as left value" in {
    val as: List[String] = List("6", "foo")
    val f: String => Either[Exception, Int] =
      s => Either.Try(s.toInt)

    Either.traverse(as)(f) match {
      case Left(e: Exception) => assert(e.getMessage() == "For input string: \"foo\"")
      case _ => fail("expected left value")
    }

  }

  "Either.sequence" should "return list in right value if all are right values" in {
    val as: List[Either[String, Int]] = List(Right(6), Right(7), Right(42))
    val expected: List[Int] = List(6, 7, 42)

    Either.sequence(as) match {
      case Right(bs) => assert(bs == expected)
      case _ => fail("expected right value")
    }
  }

  it should "return first left value if any" in {
    val as: List[Either[String, Int]] = List(Right(6), Right(7), Left("foo"))

    Either.sequence(as) match {
      case Left(s) => assert(s == "foo")
      case _ => fail("expected left value")
    }
  }
}
