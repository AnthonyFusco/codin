package com.utils

case class BalrogInputs(position: Coordinate, item: EntityType, mineCount: Int) extends BaseInputs

class Balrog extends Overmind {
  override val strategy: Strategy = b => r => {
    val currentInputs = BalrogInputs(Coordinate(r.pos.x, r.pos.y), r.item, b.mineCount)
    val target = b.getClosestSingleOre(r)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case BalrogInputs(Coordinate(0, _), NOTHING, mine) if mine <= 2 => Action.request(TRAP)

      case BalrogInputs(_, TRAP, _) if target.nonEmpty => Action.dig(target.get)

      case BalrogInputs(c, _, _) if b.myTrapPos.contains(c)
      => b.mineCount = b.mineCount + 1
        b.oreStates = b.oreStates.map(o => if (o._1 == target.get) (o._1, false) else o)
        new Dwarf().strategy(b)(r)

      case BalrogInputs(_, TRAP, _) => {
        new Dwarf().strategy(b)(r)
      }

      case _ => Action.move(Coordinate(9, r.pos.y))
    }
  }
}
