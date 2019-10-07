package com.utils

import java.util.Scanner

import scala.collection.JavaConverters._
import scala.math.abs

class Cell {
  var known = false
  var ore = 0
  var hole = false

  def this(known: Boolean, ore: Int, hole: Boolean) {
    this()
    this.known = known
    this.ore = ore
    this.hole = hole
  }

  def this(in: Scanner) {
    this()
    val oreStr = in.next()
    if (oreStr.charAt(0) == '?') {
      known = false
      ore = 0
    } else {
      known = true
      ore = Integer.parseInt(oreStr)
    }
    val holeStr = in.next()
    hole = holeStr.charAt(0) != '0'
  }
}

object Action {
  def none = new Action("WAIT", null, null)

  def move(pos: Coord) = new Action("MOVE", pos, null)

  def dig(pos: Coord) = new Action("DIG", pos, null)

  def request(item: EntityType) = new Action("REQUEST", null, item)
}

class Action private(val command: String, val pos: Coord, val item: EntityType) {
  var message: String = _

  override def toString: String = {
    val builder = new StringBuilder(command)
    if (pos != null) builder.append(' ').append(pos.x).append(' ').append(pos.y)
    if (item != null) builder.append(' ').append(item.name)
    if (message != null) builder.append(' ').append(message)
    builder.toString
  }
}

sealed trait EntityType {
  def name: String
}

case object NOTHING extends EntityType {
  val name = "NOTHING"
}

case object ALLY_ROBOT extends EntityType {
  val name = "ALLY_ROBOT"
}

case object ENEMY_ROBOT extends EntityType {
  val name = "ENEMY_ROBOT"
}

case object RADAR extends EntityType {
  val name = "RADAR"
}

case object TRAP extends EntityType {
  val name = "TRAP"
}

case object AMADEUSIUM extends EntityType {
  val name = "AMADEUSIUM"
}

object Entity {
  def DEAD_POS = Coord(-1, -1)

  def createEntityType(i: Int): EntityType = {
    i match {
      case -1 => NOTHING
      case 0 => ALLY_ROBOT
      case 1 => ENEMY_ROBOT
      case 2 => RADAR
      case 3 => TRAP
      case 4 => AMADEUSIUM
    }
  }
}

class Entity() {
  var id: Int = 0
  var eType: EntityType = NOTHING
  var pos: Coord = _
  var item: EntityType = NOTHING
  var action: Action = _

  def this(in: Scanner) {
    this()
    id = in.nextInt
    eType = Entity.createEntityType(in.nextInt)
    pos = Coord(in.nextInt, in.nextInt)
    item = Entity.createEntityType(in.nextInt)
  }

  def isAlive: Boolean = !(Entity.DEAD_POS == pos)
}

class Team {
  var score = 0
  var robots: java.util.Collection[Entity] = _

  def readScore(in: Scanner): Unit = {
    score = in.nextInt
    robots = new java.util.ArrayList[Entity]
  }
}

case class Coord(x: Int, y: Int) {
  def distance(other: Coord): Int = abs(x - other.x) + abs(y - other.y)

  def add(other: Coord) = Coord(x + other.x, y + other.y)
}

object Board {
  var in: Scanner = _
  var width: Int = _
  var height: Int = _
  val myTeam = new Team
  val opponentTeam = new Team
  var cells: Array[Array[Cell]] = _
  var myRadarCooldown = 0
  var myTrapCooldown = 0
  var entitiesById: java.util.Map[Integer, Entity] = _
  var myRadarPos: Vector[Coord] = _
  var myTrapPos: Vector[Coord] = _
  var ores: Vector[Coord] = _
  var radarCoords: List[Coord] = List(
    Coord(5, 3),
    Coord(9, 7),
    Coord(13, 11),
    Coord(13, 3),
    Coord(5, 11),
    Coord(17, 7),
    Coord(21, 3),
    Coord(21, 11),
    Coord(25, 7),
    Coord(28, 12),
    Coord(27, 1)
  )
  var n = 0
  var myHoles: Set[Coord] = Set()
  var enemyHoles: Vector[Coord] = _
  var holes: Vector[Coord] = _
  var orePrediction: Map[Coord, Int] = Map()
  var globalOreCount: Map[Coord, Int] = Map()

  def update(in: Scanner): Unit = {
    myTeam.readScore(in)
    opponentTeam.readScore(in)
    cells = Array.ofDim[Cell](height, width)
    ores = Vector()

    enemyHoles = Vector()
    holes = Vector()

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        cells(y)(x) = new Cell(in)
        globalOreCount = globalOreCount + (Coord(x, y) -> cells(y)(x).ore)
        if (cells(y)(x).hole) holes = holes :+ Coord(x, y)
        if (cells(y)(x).ore > 0) ores = ores :+ Coord(x, y)
      }
    }

    enemyHoles = holes diff myHoles.toList
    val diffOrePicks = orePrediction.filter(p => globalOreCount(p._1) < p._2).keys.toVector
    System.err.println(s"diffOrePicks: $diffOrePicks")
    enemyHoles = enemyHoles ++ diffOrePicks

    val entityCount = in.nextInt
    myRadarCooldown = in.nextInt
    myTrapCooldown = in.nextInt
    entitiesById = new java.util.HashMap[Integer, Entity]
    myRadarPos = Vector()
    myTrapPos = Vector()

    for (_ <- 0 until entityCount) {
      val entity: Entity = new Entity(in)
      entitiesById.put(entity.id, entity)
      entity.eType match {
        case ALLY_ROBOT => myTeam.robots.add(entity)
        case ENEMY_ROBOT => opponentTeam.robots.add(entity)
        case RADAR => myRadarPos = myRadarPos :+ Coord(entity.pos.x, entity.pos.y)
        case TRAP => myTrapPos = myTrapPos :+ Coord(entity.pos.x, entity.pos.y)
        case AMADEUSIUM =>
        case NOTHING =>
      }
    }
    n = n + 1
  }


  def cellExist(pos: Coord): Boolean = (pos.x >= 0) && (pos.y >= 0) && (pos.x < width) && (pos.y < height)

  def getCell(pos: Coord): Cell = cells(pos.y)(pos.x)
}