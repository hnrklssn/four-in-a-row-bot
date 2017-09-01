package learning

import logic.{BoardStateRater, PlayMaker}
import model._

import scala.annotation.tailrec

/**
  * Created by henrik on 2017-08-18.
  */
class Tournament(persistence: Persistence, playMakers: PlayMaker*) {

  def runMatch(player1: PlayMaker, player2: PlayMaker): Option[PlayMaker] = {
    val board = EmptyBoard
    makeTurns((player1, Player1Marker), (player2, Player2Marker), board).map{
      case Player1Marker => player1
      case Player2Marker => player2
    } //Player1Marker always starts
  }

  private def makeTurns(currentPlayer: (PlayMaker, Player), otherPlayer: (PlayMaker, Player), board: Board): Option[Player] = {
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
      a <- playMakers
      b <- playMakers.filter(_ != a)
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
      val victor = victorOpt.map { case (b, _) => b.toString }.getOrElse("draw")
      println(s"$bot1 vs $bot2 - winner: $victor")
    }
  }
}

