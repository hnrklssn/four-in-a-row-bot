package com.hnrklssn.fourinarow.core.model

import BoardStateRater._
import com.hnrklssn.fourinarow.core.util.Math

import scala.annotation.tailrec

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

  override def toString: String = {
    val colStrings = (0 to 6).map{x =>
      val c = Seq.fill(6 - col(x).size)(EmptySpace) ++ col(x)
      c.tail.foldLeft(c.head.toString)((acc, m) => s"$acc,$m")}
    colStrings.tail.foldLeft(colStrings.head)((acc,s) => s"$acc,$s")
  }


}

class BoardImpl(input: Seq[List[Marker]], lastPlacedOption: Option[Int] = None) extends Board {

  def placeMarker(x: Int, marker: Marker): Board = {
    val newArray = input.zipWithIndex
      .map{ case(xs, i) =>
        if(i == x) {
          marker :: xs
        }  else {
          xs
        }
      }
    new BoardImpl(newArray, Some(x))
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
    lastPlacedOption.fold(blindVictoryCheck(player)) { lastPlacedCol =>
      col(lastPlacedCol).head == player && smartVictoryCheck(player, lastPlacedCol)
    }

  }

  private def blindVictoryCheck(player: Player): Boolean = {
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

  private def smartVictoryCheck(player: Player, lastPlacedCol: Int): Boolean = {
    System.err.println("smartVicotryCheck " + player:)
    val lastPlacedRow = col(lastPlacedCol).size - 1

    {
      val colStreak = Board.longestStreak(col(lastPlacedCol), player)
      colStreak >= 4
    } || {
      val rowStreak = Board.longestStreak(row(lastPlacedRow), player)
      rowStreak >= 4
    } || {
      val leftDiagonal =
        (for{
          i <- 0 to 6
          j = lastPlacedRow + (lastPlacedCol - i)
        } yield if(j < 6 && j >= 0) {
          Some(i -> j)
        } else {
          None
        }).collect{ case Some((i, j)) => apply(i)(j)}
      val leftDiagonalStreak = Board.longestStreak(leftDiagonal, player)
      leftDiagonalStreak >= 4
    } || {
      val rightDiagonal =
        (for{
          i <- 0 to 6
          j = lastPlacedRow - (lastPlacedCol - i)
        } yield if(j < 6 && j >= 0) {
          Some(i -> j)
        } else {
          None
        }).collect{case Some((i, j)) => apply(i)(j)}
      val rightDiagonalStreak = Board.longestStreak(rightDiagonal, player)
      rightDiagonalStreak >= 4
    }
  }

  override def isDraw(): Boolean = input.forall(_.lengthCompare(6) == 0) && !(isVictorious(Player1Marker) || isVictorious(Player2Marker))

  override def ended(): Boolean = input.forall(_.lengthCompare(6) == 0) || isVictorious(Player1Marker) || isVictorious(Player2Marker)
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
    new BoardImpl(array, None)
  }

  def fromRiddles(input: String): BoardImpl = {
    val array = input.split(',')
      .map{
        case "0" => Player1Marker
        case "1" => Player2Marker
        case _ => EmptySpace
      }.grouped(7).toSeq.transpose
      .map(_.filterNot(_ == EmptySpace))
      .map(_.toList)
    new BoardImpl(array, None)
  }

  @tailrec
  def longestStreak(seq: Seq[Marker], player: Player, max: Int = 0, curr: Int = 0): Int = {
    if(seq.isEmpty || seq.lengthCompare(4 - curr) < 0) {
      max
    } else {
      val next = if(seq.head == player) {
        curr + 1
      } else {
        0
      }
      if(next >= 4) {
        next
      } else {
        longestStreak(seq.tail, player, java.lang.Math.max(max, next), next)
      }
    }
  }

  def prettyPrint(board: Board): String = {
    "0|1|2|3|4|5|6\n" +
    (0 to 5).reverse.map{ y =>
      (0 to 6).map{ x => board(x)(y) }
        .mkString("|")
    }.mkString("\n")
  }
}

object EmptyBoard extends Board {
  override def placeMarker(x: Int, marker: Marker): Board = {
    val array: Array[List[Marker]] = Array.fill(7)(Nil)
    array(x) = List(marker)
    new BoardImpl(array, Some(x))
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