package com.utgk

import java.util.Scanner

import scala.collection.JavaConverters._
import com.utils.{Balrog, Board, Dwarf, Entity, Scout}

object Player extends App {
  final val in = new Scanner(System.in)
  val board = new Board(in)

  var n = 0

  while (true) {
    board.update(in)

    val scout = new Scout
    val dwarf = new Dwarf
    val balrog = new Balrog

    val scoutStrat = scout.strategy(board)
    val balrogStrat = balrog.strategy(board)
    val dwarfStrat = dwarf.strategy(board)

    board.myTeam.robots.asScala.head.action = balrogStrat(board.myTeam.robots.asScala.head)
    board.myTeam.robots.asScala.tail.head.action = scoutStrat(board.myTeam.robots.asScala.tail.head)
    board.myTeam.robots.asScala.tail.tail.foreach(r => r.action = dwarfStrat(r))

    for (robot: Entity <- board.myTeam.robots.asScala) {
      robot.action.message = robot.id.toString
      println(robot.action)
    }

    n = n + 1
  }

}
