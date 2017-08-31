package learning

import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorSystem, Props}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.global
import akka.actor.Actor.Receive
import akka.event.Logging

import scala.concurrent.duration.Duration

/**
  * Created by henrik on 2017-08-22.
  */
class Learn(path: String) extends Actor {
  import context._

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
      println(s"bot${bot.id}-${bot.version}\t-\twins: $wins\tdraws: $draws\tlosses: $losses")
    }

    val trainingData = h2Persistence.getBoards()
    val improvedLoser = sorted.head._1.train(trainingData)
    sorted.tail.map(_._1) :+ improvedLoser
  }
}

object Learn extends App {
  val startTime = System.currentTimeMillis()

  val initialNets: Seq[NeuralBoardRater] = (0 to 5).map(n => NeuralBoardRater(n))

  H2Persistence.initialize()
  val path = System.getProperty("user.home") + File.separator + "bots"
  val customDir = new File(path)
  if (customDir.exists() || customDir.mkdirs()) {
    implicit val system: ActorSystem = ActorSystem("system")
    val actorRef = system.actorOf(Props(classOf[Learn], path), "learnActor")
    actorRef ! Iterate(0, initialNets)
    scala.io.StdIn.readLine() //wait for user interruption
    actorRef ! Stop
    println("stopping actor")
    Await.ready(system.whenTerminated, Duration(5, TimeUnit.MINUTES))
  } else {
    System.err.println("Can not write weights to file. Directory does not exist. Exiting")
  }
  val endTime = System.currentTimeMillis()
  println(s"Done! - time passed: ${(endTime - startTime)/1000/60} minutes")
}

case class Iterate(i: Int, bots: Seq[NeuralBoardRater])
case object Stop
