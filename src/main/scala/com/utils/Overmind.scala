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

  val strategy: Strategy

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

  override val strategy: Strategy = b => r => {
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case Inputs(_, item) if item == EntityType.AMADEUSIUM
      => Action.move(new Coord(0, r.pos.y))

      case _ if b.oreStates.exists(_._2) =>
        val closest = b.getClosestOre(r)
        b.oreStates = b.oreStates.map(o => if (o._1 == closest && b.getCell(o._1.toCoord).ore == 1) (o._1, false) else o)
        //System.err.println(b.oreStates)
        Action.dig(closest.toCoord)

      case _ => Action.dig(Coordinate(9, r.pos.y).toCoord)
    }
  }

}

class Scout() extends Overmind {
  val radarCoords: List[Coordinate] = List(Coordinate(9,7),
    Coordinate(5,3),
    Coordinate(5,11),
    Coordinate(13,11),
    Coordinate(13,3),
    Coordinate(17,7),
    Coordinate(21,3),
    Coordinate(21,11),
    Coordinate(25,7),
  )

  override val strategy: Strategy = b => r => {
    val radarsBoardsCoordinate = b.myRadarPos.asScala.map(position => Coordinate(position.x, position.y)).toList
    val diffRadAndCurrent = radarCoords diff radarsBoardsCoordinate
    val target = diffRadAndCurrent.headOption match {
      case Some(coordinate) => coordinate
      case _ => Coordinate(-1, -1)
    }
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case Inputs(_, item) if item != EntityType.RADAR && target != Coordinate(-1, -1)
      => Action.request(EntityType.RADAR)

      case Inputs(_, item) if target != Coordinate(-1, -1) && item == EntityType.RADAR
      => Action.dig(target.toCoord)

      case _ => new Dwarf().strategy(b)(r)
    }

  }

}
