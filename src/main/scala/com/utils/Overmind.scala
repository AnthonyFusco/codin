package com.utils

import scala.math.abs

abstract class Overmind {
  def strategy(r: Entity): Action

  def oresByDistance(r: Entity): List[Coordinate] = {
    val available = Board.ores.toList
    available.map(x => (x.distance(r.pos), x)).sortBy(_._1).map(_._2)
  }

  def furthestTrapPos(r: Entity): Option[Coordinate] = {
    val oreWithoutTrap = Board.ores.toList.diff(Board.myTrapPos).diff(Board.enemyHoles)
    val furthest = oreWithoutTrap.map(x => (x.distance(r.pos), x)).sortBy(_._1).map(_._2).reverse
    var res = furthest.map(c => (c, Board.getCell(c))).find(_._2.ore == 2).map(_._1)
    res = if (res.isEmpty) furthest.map(c => (c, Board.getCell(c))).find(_._2.ore >= 2).map(_._1) else res
    if (res.isEmpty) furthest.map(c => (c, Board.getCell(c))).find(_._2.ore >= 1).map(_._1) else res
  }

  def closestTrapPos(r: Entity): Option[Coordinate] = {
    val oreWithoutTrap = Board.ores.toList.diff(Board.myTrapPos).diff(Board.enemyHoles)
    val furthest = oreWithoutTrap.map(x => (x.distance(r.pos), x)).sortBy(_._1).map(_._2)
    var res = furthest.map(c => (c, Board.getCell(c))).find(_._2.ore == 2).map(_._1)
    res = if (res.isEmpty) furthest.map(c => (c, Board.getCell(c))).find(_._2.ore >= 2).map(_._1) else res
    if (res.isEmpty) furthest.map(c => (c, Board.getCell(c))).find(_._2.ore >= 1).map(_._1) else res
  }

  def closestSafeOre(r: Entity): Option[Coordinate] = {
    oresByDistance(r).diff(Board.myTrapPos).diff(Board.enemyHoles).headOption
  }

  def closestEnemyHole(r: Entity): Option[Coordinate] = {
    Board.enemyHoles.diff(Board.myTrapPos).map(x => (x.distance(r.pos), x)).sortBy(_._1).map(_._2).headOption
  }
}

trait BaseInputs {
  def position: Coordinate
  def item: EntityType
}

case class Inputs(position: Coordinate, item: EntityType) extends BaseInputs

case class Coordinate(x: Int, y: Int) {
  def distance(other: Coordinate): Int = abs(x - other.x) + abs(y - other.y)
  def add(other: Coordinate) = Coordinate(x + other.x, y + other.y)
}
