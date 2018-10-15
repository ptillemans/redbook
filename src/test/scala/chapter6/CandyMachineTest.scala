package chapter6

import org.scalatest.FlatSpec

class CandyMachineTest extends FlatSpec {

  "the candy machine" should "unlock when a  coin is inserted" in {
    val machine = Machine(true, 5, 10)

    val actual = machine.insertCoin
    val expected = Machine(false, 5, 11)

    assert(actual == expected)
  }

  it should "ignore coins when unlocked" in {
    val machine = Machine(false, 5, 10)

    val actual = machine.insertCoin

    assert(actual == machine)
  }

  it should "dispense candy when turned if unlocked" in {

    val machine = Machine(false, 5, 10)

    val actual = machine.turnKnob
    val expected = Machine(true, 4, 10)

    assert(actual == expected)
  }

  it should "ignore turning knob if locked" in {
    val machine = Machine(true, 5, 10)

    val actual = machine.turnKnob

    assert(actual == machine)
  }

  it should "ignore coins when out of candy" in {
    val machine = Machine(true, 0, 10)

    val actual = machine.insertCoin

    assert(actual == machine)
  }

  it should "ignore turns when out of candy" in {
    val machine = Machine(false, 0, 10)

    val actual = machine.turnKnob

    assert(actual == machine)
  }

  "simulateMachine" should "return coins and candies after actions" in {
    val machine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn)

    val actual = Machine.simulateMachine(inputs).run(machine)

    val expected = Machine(true, 3, 12)

    val (t, newMachine) = actual

    assert(t == expected.contents)
    assert(newMachine == expected)

  }

  "simulateMachine2" should "return coins and candies after actions" in {
    val machine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn)

    val actual = Machine.simulateMachine2(inputs).run(machine)

    val expected = Machine(true, 3, 12)

    val (t, newMachine) = actual

    assert(t == expected.contents)
    assert(newMachine == expected)

  }

}
