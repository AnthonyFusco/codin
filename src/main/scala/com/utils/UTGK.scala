package com.utils

import java.util.Scanner

import com.utils.EntityType.EntityType

import scala.math._

class Coord(val x: Int, val y: Int) {
  def this(in: Scanner) {
    this(in.nextInt, in.nextInt)
  }

  def add(other: Coord) = new Coord(x + other.x, y + other.y)

  // Manhattan distance (for 4 directions maps)
  // see: https://en.wikipedia.org/wiki/Taxicab_geometry
  def distance(other: Coord): Int = abs(x - other.x) + abs(y - other.y)

  override def hashCode: Int = {
    val PRIME = 31
    var result = 1
    result = PRIME * result + x
    result = PRIME * result + y
    result
  }

  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Coord]
    (x == other.x) && (y == other.y)
  }

  override def toString: String = x + " " + y
}

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
    if (pos != null) builder.append(' ').append(pos)
    if (item != null) builder.append(' ').append(item)
    if (message != null) builder.append(' ').append(message)
    builder.toString
  }
}

object EntityType extends scala.Enumeration {
  type EntityType = Value
  val NOTHING, ALLY_ROBOT, ENEMY_ROBOT, RADAR, TRAP, AMADEUSIUM = Value
  def valueOf(id: Int): EntityType = EntityType.values.find(x => x.id == id + 1).getOrElse(EntityType.NOTHING)
  implicit def convertValue(v: Value): EntityType = v.asInstanceOf[EntityType]
}

object Entity {
  def DEAD_POS = new Coord(-1, -1)
}

class Entity() {
  var id: Int = 0
  var eType: EntityType.Value = EntityType.NOTHING
  var pos: Coord = _
  var item: EntityType.Value = EntityType.NOTHING
  var action: Action = _

  def this(in: Scanner) {
    this()
    id = in.nextInt
    eType = EntityType.valueOf(in.nextInt)
    pos = new Coord(in)
    item = EntityType.valueOf(in.nextInt)
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
  // Given at startup
  val width: Int = in.nextInt
  val height: Int = in.nextInt
  // Updated each turn
  final val myTeam = new Team
  final val opponentTeam = new Team
  private var cells: Array[Array[Cell]] = _
  var myRadarCooldown = 0
  var myTrapCooldown = 0
  var entitiesById: java.util.Map[Integer, Entity] = _
  var myRadarPos: java.util.Collection[Coord] = _
  var myTrapPos: java.util.Collection[Coord] = _
  var ores: java.util.Collection[Coord] = _

  def update(in: Scanner): Unit = {
    myTeam.readScore(in)
    opponentTeam.readScore(in)
    cells = Array.ofDim[Cell](height, width)

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        cells(y)(x) = new Cell(in)
      }
    }

    val entityCount = in.nextInt
    myRadarCooldown = in.nextInt
    myTrapCooldown = in.nextInt
    entitiesById = new java.util.HashMap[Integer, Entity]
    myRadarPos = new java.util.ArrayList[Coord]
    myTrapPos = new java.util.ArrayList[Coord]

    for (_ <- 0 until entityCount) {
      val entity: Entity = new Entity(in)
      entitiesById.put(entity.id, entity)
      entity.eType match {
        case EntityType.ALLY_ROBOT => myTeam.robots.add(entity)
        case EntityType.ENEMY_ROBOT => opponentTeam.robots.add(entity)
        case EntityType.RADAR => myRadarPos.add(entity.pos)
        case EntityType.TRAP => myTrapPos.add(entity.pos)
        case EntityType.AMADEUSIUM => ores.add(entity.pos)
      }
    }
  }

  def cellExist(pos: Coord): Boolean = (pos.x >= 0) && (pos.y >= 0) && (pos.x < width) && (pos.y < height)

  def getCell(pos: Coord): Cell = cells(pos.y)(pos.x)
}