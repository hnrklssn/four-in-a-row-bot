package learning

import logic.HumanPlayer

import scala.io.StdIn.readLine

/**
  * Created by henrik on 2017-08-31.
  */
object Play extends App {
  println("Enter your name")
  val name = readLine()
  val player = new HumanPlayer(name)
  println("Enter file path to load net from")
  val input = readLine()
  val ai = NeuralBoardRater.fromFile(input, explore = false)
  val tournament = new Tournament(MockPersistence, player, ai)
  tournament.run()
}
