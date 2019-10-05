package com.utils

import EntityType.EntityType

import scala.collection.JavaConverters._
import scala.math.abs

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
  val getOres: (Board => List[OreState]) = b =>
    if (b.ores == null) List()
    else b.ores.asScala.toList.map(c => (Coordinate(c.x, c.y), false))

  def selectNextOrePos(board: Board): Coord =
    board.ores.asScala match {
      case head :: _ => head
      case Nil => Entity.DEAD_POS
    }

}

case class Inputs(position: Coordinate, item: EntityType)

case class Coordinate(x: Int, y: Int) {
  def toCoord: Coord = new Coord(x, y)
  def distance(other: Coordinate): Int = abs(x - other.x) + abs(y - other.y)
}

class Dwarf() extends Overmind {

  def getClosestOre(ores: List[OreState], r: Entity): Coordinate = {
    val available = ores.filter(!_._2).map(_._1)
    available.map(x => (x.distance(r.posCoordinate), x)).minBy(_._1)._2
  }

  override val strategy: Strategy = b => r => {
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    val ores = getOres(b)
    currentInputs match {
      case Inputs(_, item) if item == EntityType.AMADEUSIUM
      => Action.move(new Coord(0, r.pos.y))

      case Inputs(position, _) if b.getCell(position.toCoord).ore != 0
      => Action.dig(position.toCoord)

      case _ if ores.nonEmpty => Action.dig(getClosestOre(ores, r).toCoord)

      case _ => Action.none
    }
  }

}

class Scout() extends Overmind {
  override val strategy: Strategy = b => r => {
    val yPos = r.pos.y
    val target = Coordinate(15, 8)
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case Inputs(_, item) if item != EntityType.RADAR
      => Action.request(EntityType.RADAR)

      case Inputs(position, item) if position != target && item.equals(EntityType.RADAR)
      => Action.move(target.toCoord)

      case Inputs(Coordinate(15, 8), EntityType.RADAR)
      => Action.dig(target.toCoord)

      case _ => new Dwarf().strategy(b)(r)
    }

  }

}
