package chapter6

import org.scalatest.FlatSpec
import chapter6.Random

class RandomTest extends FlatSpec {

  "nonNegativeInt" should "return positive integers" in {
    val rng =  SimpleRNG(1059025964525L)
    val (n, nextRng) = Random.nonNegativeInt.run(rng)
    assert(n > 0)
  }

  "double" should "return floats between 0 and 1" in {
    val r = 0 until 100
    var rng: RNG = SimpleRNG(42)
    var x: Double = 0.0

    r.foreach(_ => {
                val (x, nextRng) = Random.double.run(rng)
                assert(x >= 0 &&  x <= 1.0)
                rng = nextRng
              })
  }


}
