package logic

import model.{Board, Marker, Player}

import scala.util.Random

/**
  * Created by henrik on 2017-08-16.
  */
trait BoardStateRater {
  def rate(board: Board, player: Player): Option[Double]
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
      (Math.max(max, next), next)
    }._1
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
}

object BoardStateRater {
  def flipRating(r: Double): Double = 1 - r
}