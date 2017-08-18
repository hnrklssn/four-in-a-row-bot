import Logic.BoardStateRater
import model._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by henrik on 2017-08-15.
  */

class ModelTest extends FlatSpec with Matchers {
  "A Board" should "store an empty board" in {
    val inputString = Array.fill(6*7)(".").mkString(",")
    val board = Board(inputString)
    for {
      x <- 0 until 7
      y <- 0 until 6
    } assert(board(x)(y) == EmptySpace)
  }

  it should "store a non empty board" in {
    val inputString = ".,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,0,0,.,.,.,.,.,1"
    val board = Board(inputString)
    for {
      x <- 0 until 7
      y <- 0 until 6
    } {
      val marker = (x, y) match {
        case (6,0) => Player2Marker
        case (5,0) => Player1Marker
        case (5,1) => Player1Marker
        case _ => EmptySpace
      }
      assert(board(x)(y) == marker)
    }
  }

  it should "place a new marker" in {
    val inputString = ".,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,0,0,.,.,.,.,.,1"
    val board = Board(inputString)
    val nextBoard = board.placeMarker(5, Player2Marker)
    for {
      x <- 0 until 7
      y <- 0 until 6
    } {
      val marker = (x, y) match {
        case (6,0) => Player2Marker
        case (5,0) => Player1Marker
        case (5,1) => Player1Marker
        case (5,2) => Player2Marker
        case _ => EmptySpace
      }
      assert(nextBoard(x)(y) == marker)
    }
  }

  it should "not affect the state of the current board when placing a new marker" in {
    val inputString = ".,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,0,0,.,.,.,.,.,1"
    val board = Board(inputString)
    val nextBoard = board.placeMarker(5, Player2Marker)
    for {
      x <- 0 until 7
      y <- 0 until 6
    } {
      val marker = (x, y) match {
        case (6,0) => Player2Marker
        case (5,0) => Player1Marker
        case (5,1) => Player1Marker
        case _ => EmptySpace
      }
      assert(board(x)(y) == marker)
    }
  }

  it should "retrieve a column" in {
    val inputString = ".,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,0,0,.,.,.,.,.,1"
    val board = Board(inputString)
    val column = board.col(5)
    column should contain theSameElementsAs List(Player1Marker, Player1Marker)
  }

  "EmptyBoard" should "store an empty board" in {
    val board = EmptyBoard
    for {
      x <- 0 until 7
      y <- 0 until 6
    } assert(board(x)(y) == EmptySpace)
  }

  it should "place a new marker" in {
    val board = EmptyBoard
    val nextBoard = board.placeMarker(5, Player1Marker)
    for {
      x <- 0 until 7
      y <- 0 until 6
    } {
      val marker = (x, y) match {
        case (5,0) => Player1Marker
        case _ => EmptySpace
      }
      assert(nextBoard(x)(y) == marker)
    }
  }

  "Any board" should "make a move according to board state rating" in {
    val board =  EmptyBoard
    val middleRater = new BoardStateRater {
      override def rate(board: Board, player: Player): Option[Double] = Some(board.col(3).count(_ == player))
    }
    val move = board.bestMove(middleRater, Player1Marker)
    assert(move == 3)
  }
}
