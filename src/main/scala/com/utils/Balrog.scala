package com.utils

object Balrog extends Overmind {

  val TRAP_LIMIT = 6
  case class BalrogInputs(currentMission: Option[Action], position: Coord,
                          item: EntityType, mineCount: Int,
                          trapTarget: Option[Coord]) extends BaseInputs
  var trapRequested: Boolean = false
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
    val currentInputs = BalrogInputs(currentMission, r.pos, r.item, Board.myTrapPos.length, nextClosestTrapPos(r))
    if (r.item == TRAP) trapRequested = false

    currentInputs match {
      case _ if !r.isAlive => Action.none

      case BalrogInputs(Some(mission), _, TRAP, _, _) =>
        if (Board.enemyHoles.contains(mission.pos)) {
          currentMission = None
          return Dwarf.strategy(r)
        }
        mission

      case BalrogInputs(None, Coord(0, _), NOTHING, _, _)
        if !trapRequested && Board.myTrapCooldown == 0 && Board.myTrapPos.length <= TRAP_LIMIT =>
        trapRequested = true
        Action.request(TRAP)

      case BalrogInputs(None, _, TRAP, _, Some(target)) =>
        currentMission = Some(Action.dig(target))
        registerPickup(target)
        Action.dig(target)

      case BalrogInputs(_, _, item, _, _) if item != TRAP => Dwarf.strategy(r)

      case _ => Action.request(RADAR)
    }
  }
}
