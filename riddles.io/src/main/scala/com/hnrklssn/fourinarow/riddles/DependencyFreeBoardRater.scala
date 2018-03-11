package com.hnrklssn.fourinarow.riddles

import com.hnrklssn.fourinarow.core.model._

import scala.io.Source

class DependencyFreeBoardRater(net: DependencyFreeNet) extends BoardStateRater {
  override def rate(board: Board, player: Player): Option[Double] = if(board.ended()){
    Some(if(board.isVictorious(player)) {
      System.err.println(s"victorious! $player")
      1.0
    } else if(board.isVictorious(player.otherPlayer)) {
      System.err.println(s"loser: $player")
      0.0
    } else {
      0.5
    })
  } else {
    Option(net(boardToVector(board, player))(0))
  }

  private def boardToVector(board: Board, player: Player): Vector[Double] = {
    val boardString = player match {
      case Player1Marker => board.toString
      case Player2Marker => board.toString.map{
        case '0' => '1'
        case '1' => '0'
        case c: Char  => c
      }
    }
    boardStringToVector(boardString)
  }

  private def boardStringToVector(board: String): Vector[Double] = {
    board.split(',').map{
      case "0" => 1.0
      case "1" => -1.0
      case "." => 0.0
    }.toVector
  }

  override def id: Int = 1

  override def version: Int = 1

  override val random: Boolean = false
}

object DependencyFreeBoardRater {
  def fromFile(file: String): DependencyFreeBoardRater = {
    val content = Source.fromFile(file).mkString
    new DependencyFreeBoardRater(DependencyFreeNet(content))
  }
}
