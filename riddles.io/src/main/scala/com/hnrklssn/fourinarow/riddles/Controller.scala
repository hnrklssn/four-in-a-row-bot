package com.hnrklssn.fourinarow.riddles

import com.hnrklssn.fourinarow.core.model.{Board, BoardStateRater, Player, State}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
  * Created by henrik on 2017-08-15.
  */
class Controller {

  private lazy val weightString: String = {
    val size = WeightsConfigContainer.weights.length
    val index = Random.nextInt(size)
    WeightsConfigContainer.weights(index)
  }

  private val boardStateRater: BoardStateRater = new DependencyFreeBoardRater(DependencyFreeNet(weightString))

  @tailrec
  final def run(state: State, inputStream: Stream[String]): Unit = {
    System.err.println("running")
    if(inputStream.nonEmpty) {
      System.err.println("inside if")
      val line = inputStream.head
      System.err.println("scanned line")
      if (line.length() != 0) {
        val parts = line.split(" ")
        parts(0) match {
          case "update" =>
            if(parts(2) == "field") {
              run(state.updateBoard(Board(parts(3))), inputStream.tail)
            } else {
              run(state, inputStream.tail)
            }
          case "action" =>
            val playerMarker = Player(state.yourBotId)
            val move = boardStateRater.pickMove(state.board, playerMarker)
            System.out.println(s"place_disc $move")
            System.out.flush()
            run(state, inputStream.tail)
          case _ => ()
        }
      }
    } else {
      System.err.println("no more input, exiting")
    }
  }

}

object Controller {
  def main(args: Array[String]) {
    System.err.println("starting")
    val inputStream = Source.stdin.getLines().toStream
    val settingsMap = inputStream
      .takeWhile(_.startsWith("setting"))
      .collect(State.readSetting)
      .toMap

    val timeBankSetting = settingsMap(State.timeBankLabel).toInt
    val timePerMoveSetting = settingsMap(State.timePerMoveLabel).toInt
    val yourBotSetting = settingsMap(State.yourBotLabel)
    val yourBotIdSetting = settingsMap(State.yourBotIdLabel).toInt

    val settings = State(maxTimeBank = timeBankSetting,
      timePerMove = timePerMoveSetting,
      yourBot = yourBotSetting,
      yourBotId = yourBotIdSetting)

    (new Controller).run(settings, inputStream.dropWhile(_.startsWith("setting")))
  }
}


