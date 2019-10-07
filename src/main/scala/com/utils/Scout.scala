package com.utils
import scala.collection.JavaConverters._

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
    val radarsBoardsCoordinate = b.myRadarPos.map(position => Coordinate(position.x, position.y)).toList
    val diffRadAndCurrent = radarCoords diff radarsBoardsCoordinate
    val target = diffRadAndCurrent.headOption match {
      case Some(coordinate) => coordinate
      case _ => Coordinate(-1, -1)
    }
    val currentInputs = Inputs(Coordinate(r.pos.x, r.pos.y), r.item)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case Inputs(_, NOTHING) if target != Coordinate(-1, -1) => Action.request(RADAR)

      case Inputs(_, RADAR) if target != Coordinate(-1, -1) => Action.dig(target)

      case _ => new Dwarf().strategy(b)(r)
    }

  }

}
