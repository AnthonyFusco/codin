package com.utils

object Scout extends Overmind {

  var radarRequested: Boolean = false;
  case class ScoutInputs(position: Coordinate, item: EntityType, target: Option[Coordinate]) extends BaseInputs

  def nextRadarPos(): Option[Coordinate] ={
    val radarsBoardsCoordinate = Board.myRadarPos.map(position => Coordinate(position.x, position.y)).toList
    val diffRadAndCurrent = Board.radarCoords diff radarsBoardsCoordinate
    diffRadAndCurrent.headOption match {
      case Some(target) => if (Board.enemyHoles.contains(target)) Some(target.add(Coordinate(1, 0))) else Some(target)
      case None => None
    }
  }

  override def strategy(r: Entity): Action = {
    val currentInputs = ScoutInputs(r.pos, r.item, nextRadarPos())
    if (r.item == RADAR) radarRequested = false

    currentInputs match {
      case _ if !r.isAlive => Action.none

      case ScoutInputs(Coordinate(0, _), NOTHING, Some(_)) if !radarRequested && Board.myRadarCooldown == 0 =>
        radarRequested = true
        Action.request(RADAR)

      case ScoutInputs(_, RADAR, Some(target)) => Action.dig(target)

      case _ => Balrog.strategy(r)
    }

  }

}
