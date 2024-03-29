package com.utils

object Scout extends Overmind {

  var radarRequested: Boolean = false

  case class ScoutInputs(position: Coord, item: EntityType, target: Option[Coord]) extends BaseInputs

  def nextRadarPos(): Option[Coord] = {
    val diffRadAndCurrent = Board.radarCoords diff Board.myRadarPos
    diffRadAndCurrent.headOption match {
      case Some(target) =>
        if (Board.enemyHoles.contains(target)) { //dodge trap on radar site once
          val newTarget = target.add(Coord(1, 0))
          Board.radarCoords = Board.radarCoords.map(c => if (c == target) newTarget else c)
          Some(newTarget)
        } else
          Some(target)
      case None => None
    }
  }

  override def strategy(r: Entity): Action = {
    val currentInputs = ScoutInputs(r.pos, r.item, nextRadarPos())
    if (r.item == RADAR) radarRequested = false

    currentInputs match {
      case _ if !r.isAlive => Action.none

      case ScoutInputs(Coord(0, _), NOTHING, Some(_)) if !radarRequested && Board.myRadarCooldown == 0 =>
        radarRequested = true
        Action.request(RADAR)

      case ScoutInputs(_, RADAR, Some(target)) =>
        registerPickup(target)
        Action.dig(target)

      case _ => Balrog.strategy(r)
    }

  }

}
