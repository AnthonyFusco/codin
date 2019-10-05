package com.utgk

import java.util.Scanner

import scala.collection.JavaConverters._
import com.utils.{Board, Dwarf, Entity, Scout}

object Player extends App {
  final val in = new Scanner(System.in)
  val board = new Board(in)

  var n = 0

  while (true) {
    board.update(in)

    val scout = new Scout
    val dwarf = new Dwarf

    val scoutStrat = scout.strategy(board);
    val dwarfStrat = dwarf.strategy(board);

    board.myTeam.robots.asScala.head.action = scoutStrat(board.myTeam.robots.asScala.head)
    board.myTeam.robots.asScala.tail.foreach(r => r.action = dwarfStrat(r))

    for (robot: Entity <- board.myTeam.robots.asScala) {
      robot.action.message = robot.id.toString
    }

    for (robot: Entity <- board.myTeam.robots.asScala) {
      println(robot.action)
    }
    n = n + 1
  }

}
