package com.utils

class Balrog extends Overmind {
  override val strategy: Strategy = b => r => {
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    val target = b.getClosestSingleOre(r)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case Inputs(Coordinate(0, _), item) if b.mineCount <= 2 && item == EntityType.NOTHING
      => Action.request(EntityType.TRAP)

      case Inputs(_, item) if target.nonEmpty && item == EntityType.TRAP
      => b.mineCount = b.mineCount + 1
        b.oreStates = b.oreStates.map(o => if (o._1 == target.get) (o._1, false) else o)
        //System.err.println(b.oreStates)
        Action.dig(target.get.toCoord)

      case Inputs(_, item) if item != EntityType.TRAP => {
        new Dwarf().strategy(b)(r)
      }

      case _ => Action.move(Coordinate(9, r.pos.y).toCoord)
    }
  }
}
