package com.hnrklssn.fourinarow.learning

import com.hnrklssn.fourinarow.core.model.{BoardStateRater, HumanPlayer}
import com.hnrklssn.fourinarow.riddles.DependencyFreeBoardRater

import scala.io.StdIn.readLine

/**
  * Created by henrik on 2017-08-31.
  */
object Play extends App {
  val name = readLine("Enter your name")
  val player = new HumanPlayer(name)
  val path = readLine("Enter file path to load net from")
  val boardRaterType = readLine("NeuralBoardRater or DependencyFreeBoardRater? (N/D)")
  val ai: BoardStateRater = boardRaterType match {
    case "N" => NeuralBoardRater.fromFile(path, explore = false)
    case "D" => DependencyFreeBoardRater.fromFile(path)
    case s => throw new IllegalArgumentException("Expected N or D, got: " + s)
  }
  val tournament = new Tournament(MockPersistence, player, ai)
  tournament.run()
}
