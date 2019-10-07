package com.utils
import scala.collection.JavaConverters._

class Dwarf() extends Overmind {

  override val strategy: Strategy = b => r => {
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case Inputs(_, AMADEUSIUM) => Action.move(Coordinate(0, r.pos.y))

      case _ if b.oreStates.exists(_._2) =>
        val closest = b.getClosestOre(r)
        b.oreStates = b.oreStates.map(o => if (o._1 == closest && b.getCell(o._1).ore == 1) (o._1, false) else o)
        System.err.println(b.oreStates)
        Action.dig(closest)

      case _ => Action.dig(Coordinate(9, r.pos.y))
    }
  }

}
