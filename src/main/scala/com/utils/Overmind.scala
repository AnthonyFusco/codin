package com.utils

import EntityType.EntityType
import scala.collection.JavaConverters._

/*
- Associer un comportement a un bot
  - scout
  - mineur

- Plusieurs type d'IA organises les bots en se parlant entre elles
 */
abstract class Overmind() {
  type Strategy = (Board => Entity => Action)
  type OreState = (Coordinate, Boolean)
  val strategy: Strategy
  val ores: (Board => List[OreState]) = b => b.ores.asScala.toList.map(c => (Coordinate(c.x, c.y), false))

  def selectNextOrePos(board: Board): Coord =
    board.ores.asScala match {
      case head :: _ => head
      case Nil => Entity.DEAD_POS
    }

}

case class Inputs(position: Coordinate, item: EntityType)

case class Coordinate(x: Int, y: Int) {
  def toCoord: Coord = new Coord(x, y)
}

class Dwarf() extends Overmind {
  override val strategy: Strategy = b => r => {
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case Inputs(_, item) if item == EntityType.AMADEUSIUM
      => Action.move(new Coord(0, r.pos.y))

      case Inputs(position, _) if b.getCell(position.toCoord).ore != 0
      => Action.dig(position.toCoord)

      //case _ if selectNextOrePos(b) => Action.dig(selectNextOrePos(b))

      case _ => Action.none
    }
  }

}

class Scout() extends Overmind {
  override val strategy: Strategy = b => r => {
    val yPos = r.pos.y
    val target = Coordinate(5, r.pos.y)
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case Inputs(_, item) if item != EntityType.RADAR
      => Action.request(EntityType.RADAR)

      case Inputs(position, item) if position != target && item.equals(EntityType.RADAR)
      => Action.move(target.toCoord)

      case Inputs(Coordinate(5, yPos), EntityType.RADAR)
      => Action.dig(target.toCoord)

      case _ => new Dwarf().strategy(b)(r)
    }

  }

}
