package com.hnrklssn.fourinarow.core.model

import com.hnrklssn.fourinarow.core.model.BoardStateRater.flipRating
import com.hnrklssn.fourinarow.core.util.Math

import scala.util.Random

sealed trait PlayMaker {
  def pickMove(board: Board, player: Player): Int
}

/**
  * Created by henrik on 2017-08-16.
  */
trait BoardStateRater extends PlayMaker {
  def rate(board: Board, player: Player): Option[Double]
  def id: Int
  def version: Int
  val random: Boolean

  override def pickMove(board: Board, player: Player): Int = recurseBestMove(board,player, 0)._1

  private def recurseBestMove(board: Board, player: Player, level: Int): (Int, Double) = {
    //System.err.println(s"recursing - level: $level")
    val ratings = (0 -> 0.0) +: //default if filter clears everything
      (0 to 6).map(x => x -> board.col(x)).par
      .filter(_._2.lengthCompare(6) < 0)
      .map { case (x, _) =>
        val nextBoard = board.placeMarker(x, player)
        val ratingOption = this.rate(nextBoard, player)
        val rating = ratingOption match {
          case Some(r) => if(level < 3 && !nextBoard.ended()) {
            val (worstCaseMove, worstCaseRating) = this.recurseBestMove(nextBoard, player.otherPlayer, level + 1)
            if(worstCaseRating < 0 || r < 0) {
              throw new IllegalArgumentException
            }
            0.85 * r + 0.15 * flipRating(worstCaseRating)
          } else {
            r
          }
          case None =>
            val (worstCaseMove, worstCaseRating) = this.recurseBestMove(nextBoard, player.otherPlayer, level + 1)
            flipRating(worstCaseRating)
        }
        x -> rating
      }
    if(this.random) {
      Math.weightedRandomPick(ratings.seq)
    } else {
      ratings.maxBy(_._2)
    }
  }
}

object LongestStreakRater extends BoardStateRater {
  override def rate(board: Board, player: Player) = {
    val colStreaks = (0 to 6).map{x => longestStreak(board.col(x), player)}

    val rowStreaks = (0 to 5).map{ y =>
      longestStreak(board.row(y), player)
    }

    val lowerLeftBorder = (0 to 6).map(x => x -> 0) ++ (1 to 5).map(y => 0 -> y)
    val rightDiagonals = lowerLeftBorder.map{ case (x, y) =>
      (for{
        i <- x to x + 4
        j = i + (y - x)
      } yield if(i < 7 && j < 6 && i >= 0 && j >= 0) {
        Some(i -> j)
      } else {
        None
      }).collect{case Some((i, j)) => board(i)(j)}
    }
    val rightDiagonalStreaks = rightDiagonals.map{diagonal => longestStreak(diagonal, player)}

    val lowerRightBorder = (0 to 6).map(x => x -> 0) ++ (1 to 5).map(y => 6 -> y)
    val leftDiagonals = lowerRightBorder.map{ case (x, y) =>
      (for{
        i <- x - 4 to x
        j = i - (y - x)
      } yield if(i < 7 && j < 6 && i >= 0 && j >= 0) {
        Some(i -> j)
      } else {
        None
      }).collect{ case Some((i, j)) => board(i)(j)}
    }
    val leftDiagonalStreaks = leftDiagonals.map{diagonal => longestStreak(diagonal, player)}

    val allStreaks = colStreaks ++ rowStreaks ++ rightDiagonalStreaks ++ leftDiagonalStreaks
    Some(allStreaks.max)
  }

    private def longestStreak(seq: Seq[Marker], player: Player): Int = seq.foldLeft(0 -> 0){ case ((max, cur), marker) =>
      val next = if(marker == player) {
        cur + 1
      } else {
        0
      }
      java.lang.Math.max(max, next) -> next
    }._1

  override def id: Int = -1

  override def version: Int = 0

  override val random = false
}

object RandomRater extends BoardStateRater {
  private val rand: Random = Random
  override def rate(board: Board, player: Player): Option[Double] = if(rand.nextDouble() < 0.85) {
    val temp = rand.nextDouble()
    System.err.println(s"Some($temp)")
    Some(temp)
  } else {
    System.err.println("None")
    None
  }

  override def id: Int = -2

  override def version: Int = 0

  override val random = false
}

object BoardStateRater {
  def flipRating(r: Double): Double = 1 - r
}

class HumanPlayer(name: String) extends PlayMaker {
  override def pickMove(board: Board, player: Player): Int = {
    println(s"$name's turn")
    println(Board.prettyPrint(board))
    println("Make a move (0-6)")
    val choice = scala.io.StdIn.readInt()
    if(board.col(choice).size == 6) {
      println("Column full, try again")
      pickMove(board, player)
    } else if(choice < 0 || choice > 6) {
      println(s"Your pick - $choice - is not in the interval 0-6, try again")
      pickMove(board, player)
    } else {
      choice
    }
  }
}