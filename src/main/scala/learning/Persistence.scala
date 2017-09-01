package learning

import logic.{BoardStateRater, PlayMaker}
import model.{Board, Player, Player1Marker, Player2Marker}

/**
  * Created by henrik on 2017-08-21.
  */
trait Persistence {
  def recordBoardStateResult(nextBoard: Board, result: Option[Player]): Unit

  def recordMatchResult(bot1: PlayMaker, bot2: PlayMaker, victor: Option[(PlayMaker, Boolean)]): Unit

  def recordWeights(bot: NeuralBoardRater): Unit

  def matchStats(bot: NeuralBoardRater): (Int, Int, Int)

  def getBoards(): Seq[(String, Double)]
}

class H2Persistence(iteration: Int) extends Persistence {
  import doobie.imports._
  import cats._, cats.data._, cats.implicits._
  import fs2.interop.cats._

  val xa = H2Persistence.xa

  override def recordBoardStateResult(board: Board, result: Option[Player]): Unit = {
    val exists = doesBoardExist(board).option
      .transact(xa)
      .unsafePerformIO
      .getOrElse(false)
    if(!exists) {
      insertNewBoard(board).run
        .transact(xa)
        .unsafePerformIO
    }
    updateBoardStats(board, result).run
      .transact(xa)
      .unsafePerformIO
  }

  def insertNewBoard(board: Board): Update0 = {
    sql"""INSERT INTO boards (board, wins, draws, losses, iteration) VALUES (${board.toString()}, 0, 0, 0, $iteration)""".update
  }
  def doesBoardExist(board: Board): Query0[Boolean] = sql"""SELECT TRUE FROM boards WHERE board = ${board.toString()} AND iteration = $iteration""".query[Boolean]
  def updateBoardStats(board: Board, resultOption: Option[Player]): Update0 = {
    val winIncr = resultOption.fold(0)(p => if(p == Player1Marker) 1 else 0)
    val lossIncr = resultOption.fold(0)(p => if(p == Player2Marker) 1 else 0)
    val drawIncr = resultOption.fold(1)(_ => 0)
    sql"""UPDATE boards SET (wins, draws, losses) = (wins + $winIncr, draws + $drawIncr, losses + $lossIncr)
          WHERE board = ${board.toString()} AND iteration = $iteration""".update
  }

  override def recordMatchResult(bot1: PlayMaker, bot2: PlayMaker, victor: Option[(PlayMaker, Boolean)]): Unit = insertMatchRecord(bot1, bot2, victor)
    .run
    .transact(xa)
    .unsafePerformIO

  def insertMatchRecord(bot1: PlayMaker, bot2: PlayMaker, victorOpt: Option[(PlayMaker, Boolean)]): Update0 = {
    val (victor, starterWinBool) = victorOpt.map{case (v, bool) => v.toString -> bool}.getOrElse("-" -> false)
    val identifier1 = bot1.toString
    val identifier2 = bot2.toString
    sql"""INSERT INTO games (bot1, bot2, victor, starterWin, iteration)
          VALUES ($identifier1, $identifier2, $victor, $starterWinBool, $iteration)""".update
  }

  override def recordWeights(bot: NeuralBoardRater) = {
    insertBotWeights(bot)
      .run
      .transact(xa)
      .unsafePerformIO
  }
  def insertBotWeights(bot: NeuralBoardRater): Update0 = {
    sql"INSERT INTO nets (weights, identifier) VALUES (${bot.json}, ${bot.id}-${bot.version})".update
  }

  override def matchStats(bot: NeuralBoardRater): (Int, Int, Int) = {
    getMatchResults(bot.toString)
      .unique
      .transact(xa)
      .unsafePerformIO
  }
  def getMatchResults(identifier: String): Query0[(Int,Int,Int)] = sql"""SELECT
                                                                          sum(CASE WHEN victor = $identifier then 1 else 0 end),
                                                                          SUM(CASE WHEN victor = '-' then 1 else 0 end),
                                                                          SUM(CASE WHEN victor <> $identifier AND victor <> '-' then 1 else 0 end)
                                                                         FROM games
                                                                         WHERE (bot1 = $identifier OR bot2 = $identifier)
                                                                         AND iteration = $iteration"""
    .query[(Int,Int,Int)]

  override def getBoards(): Seq[(String, Double)] = {
    boards(5).vector
      .transact(xa)
      .unsafePerformIO
  }
  def boards(window: Int): Query0[(String, Double)] = //if wins + losses == 0, all games ended in draw, so we replace with 0.5
    sql"""SELECT board, COALESCE(CAST(wins AS DOUBLE) / NULLIF (wins + losses, 0), 0.5)
          FROM boards WHERE iteration > $iteration - $window """
    .query[(String, Double)]
}

object H2Persistence {

  import doobie.imports._
  import cats._, cats.data._, cats.implicits._
  import fs2.interop.cats._

  val xa = DriverManagerTransactor[IOLite](
    "org.h2.Driver", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "h2", "" //double check
  )

  def initialize() = {
    val dropBoards: Update0 =
      sql"""
    DROP TABLE IF EXISTS boards
  """.update

    val dropGames: Update0 =
      sql"""
    DROP TABLE IF EXISTS games
  """.update

    val dropNets: Update0 =
      sql"""
    DROP TABLE IF EXISTS nets
  """.update

    val createBoardTable: Update0 =
      sql"""
    CREATE TABLE boards (
      id   SERIAL,
      board VARCHAR NOT NULL,
      wins INT NOT NULL,
      draws INT NOT NULL,
      losses INT NOT NULL,
      iteration  SMALLINT NOT NULL
    )
  """.update

    val createGameTable: Update0 =
      sql"""
    CREATE TABLE games (
      id   SERIAL,
      bot1 VARCHAR NOT NULL,
      bot2 VARCHAR NOT NULL,
      victor VARCHAR NOT NULL,
      starterWin BOOLEAN NOT NULL,
      iteration INT NOT NULL
      )
  """.update

    val createNetTable: Update0 =
      sql"""
    CREATE TABLE nets (
      id   SERIAL,
      weights VARCHAR NOT NULL,
      identifier VARCHAR NOT NULL UNIQUE
    )
  """.update

    (dropBoards.run *> createBoardTable.run *> dropGames.run *> createGameTable.run *> dropNets.run *> createNetTable.run).transact(xa).unsafePerformIO
  }
}

object MockPersistence extends Persistence {
  override def recordBoardStateResult(nextBoard: Board, result: Option[Player]): Unit = ()

  override def recordMatchResult(bot1: PlayMaker, bot2: PlayMaker, victor: Option[(PlayMaker, Boolean)]): Unit = ()

  override def recordWeights(bot: NeuralBoardRater): Unit = ()

  override def matchStats(bot: NeuralBoardRater): (Int, Int, Int) = ???

  override def getBoards(): Seq[(String, Double)] = ???
}