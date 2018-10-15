package chapter6

import State.{sequence, get, set, modify}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def insertCoin: Machine =
    if (locked && (candies > 0)) Machine(false, candies, coins + 1)
    else this

  def turnKnob: Machine =
    if ((candies > 0) && !locked) Machine(true, candies - 1, coins)
    else this

  def run(action: Input): Machine =
    action match {
      case Coin => insertCoin
      case Turn => turnKnob
    }

  def contents: (Int, Int) = (coins, candies)

}

object Machine {

  def update(i: Input)(s: Machine) =
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(true, _, _)) => s.insertCoin
      case (Turn, Machine(false, _, _)) => s.turnKnob
      case _ => s
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(machine => {
            inputs.foldLeft(machine.contents, machine){(s, a) =>
              val (_, m) = s
              val nextMachine = m.run(a)
              (nextMachine.contents, nextMachine)
            }
          })

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

}
