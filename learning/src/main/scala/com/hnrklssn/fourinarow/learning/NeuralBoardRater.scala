package com.hnrklssn.fourinarow.learning

import java.io.{File, PrintWriter}

import neuroflow.application.plugin.IO
import neuroflow.core.Activator._
import neuroflow.core._
import neuroflow.nets.DenseNetwork._
import neuroflow.common.{Logs, ~>}
import shapeless._
import com.hnrklssn.fourinarow.core.model.{Board, Player, Player1Marker, Player2Marker, BoardStateRater}
import com.hnrklssn.fourinarow.core.util
import breeze.linalg.{DenseMatrix, DenseVector}
import scala.collection.parallel.ParSeq
import scala.io.{BufferedSource, Source}

/**
  * Created by henrik on 2017-08-21.
  */
class NeuralBoardRater(private val net: FeedForwardNetwork, val id: Int, val version: Int,
                       explore: Boolean, learningRateFuncGen: Option[Int => PartialFunction[(Int, Double), Double]] = None) extends BoardStateRater {

  def train(trainingData: Seq[(String, Double)]): NeuralBoardRater = {
    val (preXs, preYs) = trainingData.unzip
    val xs = preXs.map{b => boardStringToVector(b)}
    val ys = preYs.map(DenseVector(_))
    implicit val weightProvider: WeightProvider = new WeightProvider {
      override def apply(v1: Seq[Layer]): _root_.neuroflow.core.Network.Weights = net.weights
    }
    val learningRateFunc = learningRateFuncGen.map(_(version + 1)).getOrElse(net.settings.learningRate)
    val newNet = Network(NeuralBoardRater.layers, net.settings.copy(iterations = 200, learningRate = learningRateFunc))
    newNet.train(xs, ys)
    new NeuralBoardRater(newNet, id, version + 1, explore, learningRateFuncGen)
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
    Option(net.evaluate(boardToVector(board, player))(0))
  }

  private def boardToVector(board: Board, player: Player): DenseVector[Double] = {
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
  private def boardStringToVector(board: String): DenseVector[Double] = {
    DenseVector(board.split(',').map{
      case "0" => 1.0
      case "1" => -1.0
      case "." => 0.0
    })
  }

  def errorFunc(xsys: ParSeq[(String, Double)]): Double = {
    xsys.map { xy => 0.5 * math.pow(xy._2 - net.evaluate(boardStringToVector(xy._1))(0), 2) }.sum
  }

  override def equals(o: scala.Any): Boolean = o match {
    case n: NeuralBoardRater => n.net.weights.zip(net.weights)
      .forall(t =>
        (t._1 :== t._2)
          .forall(b => b)
      )
    case _ => false
  }

  override def toString: String = s"bot$id-$version"

  override val random: Boolean = explore

  override def levelLimit(numberOfEmptySpaces: Int): Int = 1 + super.levelLimit(numberOfEmptySpaces)

  def touch(): Unit = net(DenseVector.zeros(42))
}

object NeuralBoardRater {
  val f = Sigmoid
  val layers = Input(42) :: Dense(25, f) :: Output(1, f) :: HNil
  val defaultLearningRateGen: Int => PartialFunction[(Int, Double), Double] = (version: Int) => { case (i: Int, d: Double) =>
    val asymptote = if(version < 10) 1E-4 else 1E-6
    val maxFactor = 1E-3 //starting at too high of a learning rate tends to make a big leap to a plateau, making it hard to escape due to lack of gradient
  val lr = util.Math.dampenedSinusoidal(decay = 0.02, period = 2.0)(version) * //0.02 decay constant gives values > 1E-3 when version < 135
    util.Math.dampenedSinusoidal(decay = 0.01, period = 2.0)(i) * //0.01 decay constant gives values > 0.1 when i < 200
    maxFactor +
    asymptote
    println(s"learning rate: $lr version: $version iteration: $i")
    lr
  }

  def fromJson(json: String, id: Int, version: Int, explore: Boolean): NeuralBoardRater = {
    implicit val weights  = IO.Json.read(json)
    val net = Network(layers)
    new NeuralBoardRater(net, id, version, explore)
  }

  def fromFile(file: String, explore: Boolean): NeuralBoardRater = {
    val content = Source.fromFile(file).mkString
    fromString(content, explore)
  }

  def fromString(content: String, explore: Boolean): NeuralBoardRater = {
    val lines = content.split('\n')
    val split = lines.head.split('-')
    val (id, version) = (split(0).toInt, split(1).toInt)

    ~>(lines.tail.mkString("\n")).map(fromJson(_, id, version, explore))
  }

  def apply(id: Int, learningRateGen: Int => PartialFunction[(Int, Double), Double] = defaultLearningRateGen): NeuralBoardRater = {
    import neuroflow.core.FFN.WeightProvider._
    val net = Network(layers, Settings(learningRate = learningRateGen(1)))
    new NeuralBoardRater(net, id, 0, true, Some(learningRateGen)) //new board should explore during training
  }
}
