package com.utils

object Dwarf extends Overmind {

  case class DwarfInputs(position: Coordinate, item: EntityType, oreTarget: Option[Coordinate]) extends BaseInputs

  override def strategy(r: Entity): Action = {
    val currentInputs = DwarfInputs(r.pos, r.item, closestSafeOre(r))
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case DwarfInputs(_, AMADEUSIUM, _) => Action.move(Coordinate(0, r.pos.y))

      case DwarfInputs(pos, NOTHING, Some(target)) if pos.distance(target) <= 1 =>
        if (Board.enemyHoles.contains(target)) return Dwarf.strategy(r)
        Board.myHoles = Board.myHoles + target
        Action.dig(target)

      case DwarfInputs(_, NOTHING, Some(target)) => Action.move(Coordinate(target.x - 1, target.y))

      case DwarfInputs(pos, NOTHING, None) if pos.x != 0 => Action.move(Coordinate(0, r.pos.y))

      case _ => Action.dig(Coordinate(9, r.pos.y))
    }
  }

}
