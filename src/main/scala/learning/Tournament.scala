package learning

import logic.BoardStateRater
import model._

import scala.annotation.tailrec

/**
  * Created by henrik on 2017-08-18.
  */
class Tournament(persistence: Persistence, boardStateRaters: BoardStateRater*) {

  def runMatch(bot1: BoardStateRater, bot2: BoardStateRater): Option[BoardStateRater] = {
    val board = EmptyBoard
    makeTurns((bot1, Player1Marker), (bot2, Player2Marker), board).map{
      case Player1Marker => bot1
      case Player2Marker => bot2
    } //Player1Marker always starts
  }

  private def makeTurns(currentPlayer: (BoardStateRater, Player), otherPlayer: (BoardStateRater, Player), board: Board): Option[Player] = {
    val (rater, marker) = currentPlayer
    val nextBoard = board.placeMarker(board.bestMove(rater, marker), marker)
    if(nextBoard.ended()) {
      if(nextBoard.isVictorious(marker)) {
        Some(marker)
      } else {
        None
      }
    } else {
      val result = makeTurns(otherPlayer, currentPlayer, nextBoard)
      persistence.recordBoardStateResult(nextBoard, result)
      result
    }
  }

  def run(): Unit = {
    val combinations = for {
      a <- boardStateRaters.par
      b <- boardStateRaters.par.filter(_ != a)
    } yield (a, b)
    val results = combinations.zipWithIndex.map { case ((bot1, bot2),i) =>
      //println(s"Starting match $i")
      val result = runMatch(bot1, bot2)
      println(s"Finished match $i")
      (bot1, bot2, result.map(b => b -> (b == bot1))) //did the bot who started the game win?
    }
    println("All matches finished, saving results")
    results.foreach { case (bot1, bot2, victorOpt) =>
      persistence.recordMatchResult(bot1, bot2, victorOpt)
      val victor = victorOpt.map { case (b, _) => s"bot${b.id}-${b.version}" }.getOrElse("draw")
      println(s"bot${bot1.id}-${bot1.version} vs bot${bot2.id}-${bot2.version} - winner: $victor")
    }
  }
}

