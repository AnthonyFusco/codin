package com.utils

object Dwarf extends Overmind {

  case class DwarfInputs(position: Coord, item: EntityType, oreTarget: Option[Coord]) extends BaseInputs

  override def strategy(r: Entity): Action = {
    val nextTarget = if (Board.n >= 100 && closestSafeOres(r).isEmpty) {
      System.err.println("go unsafe")
      closestUnsafeOres(r)
    }
    else {
      if (closestSafeUntouchedOres(r).isEmpty ||
        closestSafeOres(r).nonEmpty && closestSafeUntouchedOres(r).head.distance(closestSafeOres(r).head) <= 4
      )
        closestSafeOres(r)
      else
        closestSafeUntouchedOres(r)
    }

    val currentInputs = DwarfInputs(r.pos, r.item, nextTarget.headOption)
    currentInputs match {
      case _ if !r.isAlive => Action.none

      case DwarfInputs(_, AMADEUSIUM, _) => Action.move(Coord(0, r.pos.y))

      case DwarfInputs(pos, NOTHING, Some(target)) if pos.distance(target) <= 1 =>
        registerPickup(target)
        Action.dig(target)

      case DwarfInputs(_, NOTHING, Some(target)) => Action.move(Coord(target.x - 1, target.y))

      case DwarfInputs(pos, NOTHING, None) if pos.x != 0 => Action.move(Coord(0, r.pos.y))

      case _ => Action.dig(Coord(1, r.pos.y))
    }
  }

}
