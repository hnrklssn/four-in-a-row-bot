package model

import logic.BoardStateRater
import logic.BoardStateRater._

/**
  * Created by henrik on 2017-08-15.
  */

trait Board {
  def placeMarker(x: Int, marker: Marker): Board
  def apply(x: Int)(y: Int): Marker
  def col(c: Int): List[Marker]
  def row(r: Int): Seq[Marker]
  def isVictorious(player: Player): Boolean
  def isDraw(): Boolean
  def ended(): Boolean
  def bestMove(boardStateRater: BoardStateRater, player: Player): Int = {
    recurseBestMove(boardStateRater, player, 0)._1
  }

  private def recurseBestMove(boardStateRater: BoardStateRater, player: Player, level: Int): (Int, Double) = {
    System.err.println(s"recursing - level: $level")
    ((0 -> -1.0) +: (0 to 6).map(x => x -> col(x))
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
      .maxBy(_._2)
  }

  override def toString: String = {
    val colStrings = (0 to 6).map{x =>
      val c = Seq.fill(6 - col(x).size)(EmptySpace) ++ col(x)
      c.tail.foldLeft(c.head.toString)((acc, m) => s"$acc,$m")}
    colStrings.tail.foldLeft(colStrings.head)((acc,s) => s"$acc,$s")
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

  override def isVictorious(player: Player): Boolean = {
    {
      val colStreaks = (0 to 6).map{x => Board.longestStreak(col(x), player)}
      colStreaks.max >= 4
    } || {
      val rowStreaks = (0 to 5).map{ y =>
        Board.longestStreak(row(y), player)
      }
      rowStreaks.max >= 4
    } || {
      val lowerRightBorder = (0 to 6).map(x => x -> 0) ++ (1 to 5).map(y => 6 -> y)
      val leftDiagonals = lowerRightBorder.map{ case (x, y) =>
        (for{
          i <- x - 6 to x
          j = y + (x - i)
        } yield if(i < 7 && j < 6 && i >= 0 && j >= 0) {
          Some(i -> j)
        } else {
          None
        }).collect{ case Some((i, j)) => apply(i)(j)}
      }
      val leftDiagonalStreaks = leftDiagonals.map{diagonal => Board.longestStreak(diagonal, player)}
      leftDiagonalStreaks.max >= 4
    } || {
      val lowerLeftBorder = (0 to 6).map(x => x -> 0) ++ (1 to 5).map(y => 0 -> y)
      val rightDiagonals = lowerLeftBorder.map{ case (x, y) =>
        (for{
          i <- x to x + 6
          j = y - (x - i)
        } yield if(i < 7 && j < 6 && i >= 0 && j >= 0) {
          Some(i -> j)
        } else {
          None
        }).collect{case Some((i, j)) => apply(i)(j)}
      }
      val rightDiagonalStreaks = rightDiagonals.map{diagonal => Board.longestStreak(diagonal, player)}
      rightDiagonalStreaks.max >= 4
    }
  }

  override def isDraw(): Boolean = input.forall(_.size == 6) && !(isVictorious(Player1Marker) || isVictorious(Player2Marker))

  override def ended(): Boolean = input.forall(_.size == 6) || isVictorious(Player1Marker) || isVictorious(Player2Marker)
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

  def longestStreak(seq: Seq[Marker], player: Player): Int = seq.foldLeft(0 -> 0){ case ((max, cur), marker) =>
    val next = if(marker == player) {
      cur + 1
    } else {
      0
    }
    (Math.max(max, next), next)
  }._1
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

  override def isVictorious(player: Player): Boolean = ???

  override def isDraw(): Boolean = ???

  override def ended(): Boolean = ???
}

sealed trait Marker
sealed trait Player extends Marker {
  def otherPlayer: Player
}

object Player1Marker extends Player {
  override def otherPlayer: Player = Player2Marker
  override def toString: String = "0"
}
object Player2Marker extends Player {
  override def otherPlayer: Player = Player1Marker
  override def toString: String = "1"
}
object EmptySpace extends Marker {
  override def toString: String = "."
}

object Player {
  def apply(p: Int) = p match {
    case 0 => Player1Marker
    case 1 => Player2Marker
  }
}