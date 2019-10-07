package com.utils

abstract class Overmind {
  def strategy(r: Entity): Action

  def oresByDistance(r: Entity): List[Coord] = {
    Board.ores.map(x => (x.distance(r.pos), x)).sortBy(_._1).map(_._2).toList
  }

  def closestSafeOres(r: Entity): List[Coord] = {
    oresByDistance(r).diff(Board.myTrapPos).diff(Board.enemyHoles)
  }

  def closestSafeUntouchedOres(r: Entity): List[Coord] = {
    oresByDistance(r).diff(Board.myTrapPos).diff(Board.enemyHoles).diff(Board.myHoles.toVector)
  }

  def closestUnsafeOres(r: Entity): List[Coord] = {
    oresByDistance(r).diff(Board.myTrapPos)
  }

  def nextClosestTrapPos(r: Entity): Option[Coord] = {
    val closest = closestSafeOres(r)
    var res = closest.map(c => (c, Board.getCell(c))).find(_._2.ore == 2).map(_._1)
    res = if (res.isEmpty) closest.map(c => (c, Board.getCell(c))).find(_._2.ore >= 2).map(_._1) else res
    if (res.isEmpty) closest.map(c => (c, Board.getCell(c))).find(_._2.ore >= 1).map(_._1) else res
  }

  def closestEnemyHole(r: Entity): Option[Coord] = {
    Board.enemyHoles.diff(Board.myTrapPos).map(x => (x.distance(r.pos), x)).sortBy(_._1).map(_._2).headOption
  }

  def registerPickup(target: Coord): Unit = {
    Board.myHoles = Board.myHoles + target
    if (!Board.orePrediction.contains(target)) {
      Board.orePrediction = Board.orePrediction + (target -> (Board.getCell(target).ore - 1))
    } else {
      Board.orePrediction = Board.orePrediction + (target -> (Board.orePrediction(target) - 1))
    }
  }
}

trait BaseInputs {
  def position: Coord

  def item: EntityType
}

case class Inputs(position: Coord, item: EntityType) extends BaseInputs
