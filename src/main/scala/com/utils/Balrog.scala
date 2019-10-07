package com.utils

import scala.util.Random

object Balrog extends Overmind {

  val TRAP_LIMIT = 3
  case class BalrogInputs(currentMission: Option[Action], position: Coordinate,
                          item: EntityType, mineCount: Int,
                          trapTarget: Option[Coordinate]) extends BaseInputs
  var trapRequested: Boolean = false;
  type Mission = Option[Action]
  var currentMission: Mission = None
  def updateMission(): Mission = {
    currentMission match {
      case Some(mission) => if(Board.myTrapPos contains mission.pos) None else Some(mission)
      case None => None
    }
  }

  override def strategy(r: Entity): Action = {
    currentMission = updateMission()
    val currentInputs = BalrogInputs(currentMission, r.pos, r.item, Board.myTrapPos.length, closestTrapPos(r))
    if (r.item == TRAP) trapRequested = false

    currentInputs match {
      case _ if !r.isAlive => Action.none

      case BalrogInputs(Some(mission), _, TRAP, _, _) => mission

      case BalrogInputs(None, Coordinate(0, _), NOTHING, _, _)
        if !trapRequested && Board.myTrapCooldown == 0 =>
        trapRequested = true
        Action.request(TRAP)

      case BalrogInputs(None, _, TRAP, _, Some(target)) =>
        currentMission = Some(Action.dig(target))
        Action.dig(target)

      case BalrogInputs(_, _, item, _, _) if item != TRAP => Dwarf.strategy(r)

      case _ => if (closestEnemyHole(r).isEmpty) Action.move(Coordinate(9, r.pos.y)) else Action.dig(closestEnemyHole(r).get)
    }
  }
}
