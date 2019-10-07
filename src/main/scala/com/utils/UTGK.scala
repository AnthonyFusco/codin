package com.utils

import java.util.Scanner
import scala.collection.JavaConverters._

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

  def move(pos: Coordinate) = new Action("MOVE", pos, null)

  def dig(pos: Coordinate) = new Action("DIG", pos, null)

  def request(item: EntityType) = new Action("REQUEST", null, item)
}

class Action private(val command: String, val pos: Coordinate, val item: EntityType) {
  var message: String = _

  override def toString: String = {
    val builder = new StringBuilder(command)
    if (pos != null) builder.append(' ').append(pos.x).append(' ').append(pos.y)
    if (item != null) builder.append(' ').append(item.name)
    if (message != null) builder.append(' ').append(message)
    builder.toString
  }
}

sealed trait EntityType { def name: String }
case object NOTHING extends EntityType { val name = "NOTHING" }
case object ALLY_ROBOT extends EntityType { val name = "ALLY_ROBOT" }
case object ENEMY_ROBOT extends EntityType { val name = "ENEMY_ROBOT" }
case object RADAR extends EntityType { val name = "RADAR" }
case object TRAP extends EntityType { val name = "TRAP" }
case object AMADEUSIUM extends EntityType { val name = "AMADEUSIUM" }

object Entity {
  def DEAD_POS = Coordinate(-1, -1)
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
  var pos: Coordinate = _
  var posCoordinate: Coordinate = _
  var item: EntityType = NOTHING
  var action: Action = _

  def this(in: Scanner) {
    this()
    id = in.nextInt
    eType = Entity.createEntityType(in.nextInt)
    pos = Coordinate(in.nextInt, in.nextInt)
    posCoordinate = Coordinate(pos.x, pos.y)
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

class Board(val in: Scanner) {
  val width: Int = in.nextInt
  val height: Int = in.nextInt
  val myTeam = new Team
  val opponentTeam = new Team
  var cells: Array[Array[Cell]] = _
  var myRadarCooldown = 0
  var myTrapCooldown = 0
  var entitiesById: java.util.Map[Integer, Entity] = _
  var myRadarPos: Vector[Coordinate] = _
  var myTrapPos: Vector[Coordinate] = _
  var ores: Vector[Coordinate] = _
  var mineCount: Int = 0
  var oreStates: Map[Coordinate, Boolean] = Map.empty

  def getOresByDistance(r: Entity): List[(Int, Coordinate)] = {
    val available = oreStates.filter(_._2).keys.toList
    available.map(x => (x.distance(r.posCoordinate), x)).sortBy(_._1)
  }

  def getClosestSingleOre(r: Entity): Option[Coordinate] = {
    getOresByDistance(r).map(o => (o._1, getCell(o._2), o._2)).find(t => t._2.ore == 2).map(_._3)
  }

  def getClosestOre(r: Entity): Coordinate = {
    getOresByDistance(r).head._2
  }

  def update(in: Scanner): Unit = {
    myTeam.readScore(in)
    opponentTeam.readScore(in)
    cells = Array.ofDim[Cell](height, width)
    ores = Vector()

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        cells(y)(x) = new Cell(in)
        if (cells(y)(x).ore > 0) {
          ores = ores :+ Coordinate(x, y)
        }
      }
    }

    ores.foreach(o => {
      if(oreStates.get(Coordinate(o.x, o.y)).isEmpty && getCell(Coordinate(o.x, o.y)).ore > 0) {
        oreStates = oreStates.updated(Coordinate(o.x, o.y), true)
      }
    })

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
        case RADAR => myRadarPos = myRadarPos :+ Coordinate(entity.pos.x, entity.pos.y)
        case TRAP => myTrapPos = myTrapPos :+ Coordinate(entity.pos.x, entity.pos.y)
        case AMADEUSIUM =>
        case NOTHING =>
      }
    }
  }

  def cellExist(pos: Coordinate): Boolean = (pos.x >= 0) && (pos.y >= 0) && (pos.x < width) && (pos.y < height)

  def getCell(pos: Coordinate): Cell = cells(pos.y)(pos.x)
}