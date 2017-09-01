package learning

import java.io.{File, PrintWriter}

import neuroflow.application.plugin.IO
import neuroflow.core.Activator._
import neuroflow.core._
import neuroflow.nets.DefaultNetwork._
import neuroflow.common.{Logs, ~>}
import shapeless._
import logic.BoardStateRater
import model.{Board, Player, Player1Marker, Player2Marker}

import scala.io.{BufferedSource, Source}

/**
  * Created by henrik on 2017-08-21.
  */
class NeuralBoardRater(private val net: FeedForwardNetwork with SupervisedTraining, val id: Int, val version: Int) extends BoardStateRater {

  def train(trainingData: Seq[(String, Double)]): NeuralBoardRater = {
    val (preXs, preYs) = trainingData.unzip
    val xs = preXs.map{b => boardStringToVector(b)}
    val ys = preYs.map(Vector(_))
    implicit val weightProvider: WeightProvider = new WeightProvider {
      override def apply(v1: Seq[Layer]): _root_.neuroflow.core.Network.Weights = net.weights
    }
    val newNet = Network(NeuralBoardRater.layers, net.settings.copy(iterations = 200))
    newNet.train(xs, ys)
    new NeuralBoardRater(newNet, id, version + 1)
  }

  lazy val json: String = IO.Json.write(net)

  def writeToFile(file: String): Unit = {

    val stringRep = s"$id-$version\n$json"

    ~> (new PrintWriter(new File(file))) io (_.write(stringRep)) io (_.close)
  }

  override def rate(board: Board, player: Player): Option[Double] = if(board.ended()){
    Some(if(board.isVictorious(player)) {
      1.0
    } else if(board.isVictorious(player.otherPlayer)) {
      0.0
    } else {
      0.5
    })
  } else {
    net.evaluate(boardToVector(board, player)).headOption
  }

  private def boardToVector(board: Board, player: Player): Vector[Double] = {
    val boardString = player match {
      case Player1Marker => board.toString
      case Player2Marker => board.toString.map{
        case '0' => '1'
        case '1' => '0'
        case c: Char  => c
      }
    }
    boardStringToVector(boardString)
  }
  private def boardStringToVector(board: String): Vector[Double] = {
    board.split(',').map{
      case "0" => 1.0
      case "1" => -1.0
      case "." => 0.0
    }.toVector
  }

  override def equals(o: scala.Any): Boolean = o match {
    case n: NeuralBoardRater => n.net.weights.deep == net.weights.deep
    case _ => false
  }

  override def toString: String = s"bot$id-$version"
}

object NeuralBoardRater {
  val f = Sigmoid
  val layers = Input(42) :: Hidden(25, f) :: Output(1, f) :: HNil

  def fromJson(json: String, id: Int, version: Int): NeuralBoardRater = {
    implicit val weights  = IO.Json.read(json)
    val net = Network(layers)
    new NeuralBoardRater(net, id, version)
  }

  def fromFile(file: String): NeuralBoardRater = {
    val lines = Source.fromFile(file).getLines().toStream
    val split = lines.head.split('-')
    val (id, version) = (split(0).toInt, split(1).toInt)

    ~>(lines.tail.mkString("\n")).map(fromJson(_, id, version))
  }

  def apply(id: Int): NeuralBoardRater = {
    import neuroflow.core.FFN.WeightProvider._
    val net = Network(layers)
    new NeuralBoardRater(net, id, 0)
  }
}
