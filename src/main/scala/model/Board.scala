package model

import Logic.BoardStateRater
import Logic.BoardStateRater._

/**
  * Created by henrik on 2017-08-15.
  */

trait Board {
  def placeMarker(x: Int, marker: Marker): Board
  def apply(x: Int)(y: Int): Marker
  def col(c: Int): List[Marker]
  def row(r: Int): Seq[Marker]
  def bestMove(boardStateRater: BoardStateRater, player: Player): Int = {
    recurseBestMove(boardStateRater, player, 0)._1
  }

  private def recurseBestMove(boardStateRater: BoardStateRater, player: Player, level: Int): (Int, Double) = {
    System.err.println(s"recursing - level: $level")
    val test = ((0 -> -1.0) +: (0 to 6).map(x => x -> col(x))
      .filter(_._2.size < 6)
      .map { t =>
        val x = t._1
        val nextBoard = placeMarker(x, player)
        val ratingOption = boardStateRater.rate(nextBoard, player)
        val rating = ratingOption match {
          case Some(r) => if(level < 3) {
            val (worstCaseMove, worstCaseRating) = nextBoard.recurseBestMove(boardStateRater, player.otherPlayer, level + 1)
            r + 0.15 * flipRating(worstCaseRating)
          } else {
            r
          }
          case None =>
            val (worstCaseMove, worstCaseRating) = nextBoard.recurseBestMove(boardStateRater, player.otherPlayer, level + 1)
            flipRating(worstCaseRating)
        }
        x -> rating
      })
      test.maxBy(_._2)
  }

}

class BoardImpl(input: Seq[List[Marker]]) extends Board {

  def placeMarker(x: Int, marker: Marker): Board = {
    val newArray = input.zipWithIndex
      .map{ t =>
        val xs = t._1
        val i = t._2
        if(i == x) {
          marker :: xs
        }  else {
          xs
        }
      }
    new BoardImpl(newArray)
  }

  def apply(x: Int)(y: Int): Marker = {
    val list = input(x)
    if(y < list.size) {
      list(list.size - y - 1)
    } else {
      EmptySpace
    }
  }

  override def col(c: Int): List[Marker] = input(c)

  override def row(r: Int): Seq[Marker] = (0 to 6).map{c => apply(c)(r)}
}

object Board {
  def apply(input: String): BoardImpl = {
    val array = input.split(',')
      .map{
        case "0" => Player1Marker
        case "1" => Player2Marker
        case _ => EmptySpace
      }.grouped(6)
      .map(_.filterNot(_ == EmptySpace))
      .map(_.toList)
      .toSeq
    new BoardImpl(array)
  }
}

object EmptyBoard extends Board {
  override def placeMarker(x: Int, marker: Marker): Board = {
    val array: Array[List[Marker]] = Array.fill(7)(Nil)
    array(x) = List(marker)
    new BoardImpl(array)
  }

  override def apply(x: Int)(y: Int): Marker = EmptySpace

  override def col(c: Int): List[Marker] = Nil

  override def row(r: Int): Seq[Marker] = Seq()
}

sealed trait Marker
sealed trait Player extends Marker {
  def otherPlayer: Player
}

object Player1Marker extends Player {
  override def otherPlayer: Player = Player2Marker
}
object Player2Marker extends Player {
  override def otherPlayer: Player = Player1Marker
}
object EmptySpace extends Marker

object Player {
  def apply(p: Int) = p match {
    case 0 => Player1Marker
    case 1 => Player2Marker
  }
}