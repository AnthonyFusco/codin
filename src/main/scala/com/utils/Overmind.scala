package com.utils

import scala.collection.JavaConverters._
import scala.math.abs

abstract class Overmind() {
  type Strategy = (Board => Entity => Action)
  val strategy: Strategy
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
