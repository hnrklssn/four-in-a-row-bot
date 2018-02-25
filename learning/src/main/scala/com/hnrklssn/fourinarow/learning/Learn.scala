package com.hnrklssn.fourinarow.learning

import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorSystem, Props}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.global
import akka.actor.Actor.Receive
import akka.event.Logging
import com.hnrklssn.fourinarow.core.model.{HumanPlayer, Player1Marker}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.collection.parallel.ParSeq
import scala.concurrent.duration.Duration

/**
  * Created by henrik on 2017-08-22.
  */
class Learn(path: String, testDataPercentage: Double) extends Actor {
  import context._
  val testRatio = 0.15

  override def receive: Receive = {
    case Iterate(i, bots) =>
      println(s"Running iteration $i")
      val persistence = new H2Persistence(i)
      val newNetsSeq = runCycle(persistence, bots)
      self ! Iterate(i + 1, newNetsSeq)
    case Stop => become(stopping)
  }

  def stopping: Receive = {
    case Iterate(i, bots) =>
      println(s"Running final iteration: $i")
      val persistence = new H2Persistence(i)
      val finalNetsSeq = runCycle(persistence, bots)
      finalNetsSeq.foreach { bot =>
        bot.writeToFile(s"$path/bot${bot.id}-${bot.version}.weights")
      }
      println("saved weights, shutting down actor system")
      stop(self)
      system.terminate()
  }

  def runCycle(h2Persistence: H2Persistence, neuralBoardRaters: Seq[NeuralBoardRater]): Seq[NeuralBoardRater] = {
    new Tournament(h2Persistence, neuralBoardRaters:_*).run()
    val sorted = neuralBoardRaters.map{ bot => bot -> h2Persistence.matchStats(bot) }
      .sortBy{ case (bot, (wins, draws, losses)) =>
        3 * wins + draws
      }
    sorted.foreach{ case (bot, (wins, draws, losses)) =>
      println(s"$bot\t-\twins: $wins\tdraws: $draws\tlosses: $losses")
    }

    val testData = h2Persistence.getBoards(15).filter{case (s, d) => decideIsTestData(s)}
    val trainingData = h2Persistence.getBoards(5).filterNot{case (s, d) => decideIsTestData(s)}
    println(s"Test data size: ${testData.size}")
    println(s"Training data size: ${trainingData.size}")
    val data = h2Persistence.getBoards(30)
    println(s"Percentage test data: ${data.count{case (s,d) => decideIsTestData(s)}.toDouble / data.size}")

    val loser = sorted.head._1
    val testDataError1 = loser.errorFunc(testData.par) / testData.size
    val trainingDataError1 = loser.errorFunc(trainingData.par) / trainingData.size

    val improvedLoser = loser.train(trainingData)
    val testDataError2 = improvedLoser.errorFunc(testData.par) / testData.size
    val trainingDataError2 = improvedLoser.errorFunc(trainingData.par) / trainingData.size

    println(s"Before: $loser - testErr: $testDataError1, trainErr: $trainingDataError1")
    println(s"After $improvedLoser - testErr: $testDataError2, trainErr: $trainingDataError2") //TODO: store in db + live plot
    sorted.tail.map(_._1) :+ improvedLoser
  }

  def decideIsTestData(s: String): Boolean = {
    //println(s"$s - hash: ${s.hashCode} pct: ${s.hashCode % 100}")
    //hashcode can be negative, resulting in negative remainder
    //don't want to abs hashcode directly in case of Int.MIN_VALUE
    math.abs(s.hashCode % 100) < testDataPercentage
  }
}

object Learn extends App {
  val startTime = System.currentTimeMillis()
  println("Enter directory path to load nets from, or leave blank to randomize new weights")
  val input = scala.io.StdIn.readLine()

  val initialNets: Seq[NeuralBoardRater] = if(input.trim == "") {
    (0 to 5).map(n => NeuralBoardRater(n))
  } else {
    loadNets(input)
  }

  H2Persistence.initialize()
  val path = System.getProperty("user.home") + File.separator + "bots" + File.separator + ISODateTimeFormat.dateHourMinute.print(DateTime.now())
  val customDir = new File(path)
  if (customDir.exists() || customDir.mkdirs()) {
    implicit val system: ActorSystem = ActorSystem("system")
    val actorRef = system.actorOf(Props(classOf[Learn], path, 15.0), "learnActor")
    actorRef ! Iterate(0, initialNets)
    scala.io.StdIn.readLine() //wait for user interruption
    actorRef ! Stop
    println("stopping actor")
    Await.ready(system.whenTerminated, Duration(50, TimeUnit.MINUTES))
  } else {
    System.err.println("Can not write weights to file. Directory does not exist. Exiting")
  }
  val endTime = System.currentTimeMillis()
  println(s"Done! - time passed: ${(endTime - startTime)/1000/60} minutes")

  def loadNets(path: String): Seq[NeuralBoardRater] = {
    val d = new File(path)
    d.listFiles
      .filter(_.isFile)
      .toList
      .map(_.getAbsolutePath)
      .map(NeuralBoardRater.fromFile(_, explore = true))
  }
}

case class Iterate(i: Int, bots: Seq[NeuralBoardRater])
case object Stop
