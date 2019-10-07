package com.utgk

import java.util.Scanner

import scala.collection.JavaConverters._
import com.utils.{Balrog, Board, Dwarf, Entity, Scout}

object Player extends App {
  final val in = new Scanner(System.in)
  Board.width = in.nextInt
  Board.height = in.nextInt
  Board.in = in

  while (true) {
    Board.update(in)

    Board.myTeam.robots.asScala.foreach(r => {
      r.action = r.id match {
        case 0 | 5 => Scout.strategy(r)
        case 1 | 6 => Scout.strategy(r)
        case 2 | 7 => Scout.strategy(r)
        case 3 | 8 => Scout.strategy(r)
        case 4 | 9 => Scout.strategy(r)
      }
    })

    //more radar in the endgame
    //count ama to know if enemy dug in your hole
    //if radar is compromised, dig next to it
    //random dig if nothing
    //if all mined when all radar up => mine unsafe
    //rapport radar/ore
    //kamikaze

    for (robot: Entity <- Board.myTeam.robots.asScala) {
      robot.action.message = robot.id.toString
      println(robot.action)
    }

  }

}
