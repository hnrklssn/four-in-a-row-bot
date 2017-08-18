package model

/**
  * Created by henrik on 2017-08-15.
  */
case class State(maxTimeBank: Int, timePerMove: Int, yourBot: String, yourBotId: Int, board: Board = EmptyBoard) {
  def yourMarker: Marker = yourBotId match {
    case 0 => Player1Marker
    case 1 => Player2Marker
  }
  def updateBoard(newBoard: BoardImpl): State = this.copy(board = newBoard)
}

object State {
  object Regex {
    val timeBank = """settings timebank (\d+)""".r("maxtime")
    val timePerMove = """settings time_per_move (\d+)""".r("movetime")
    val yourBot = """settings your_bot (.+)""".r("botname")
    val yourBotId = """settings your_botid (0|1)""".r("botid")
  }
  def readSetting: PartialFunction[String, (Symbol, String)] = {
    case Regex.timeBank(res) => (timeBankLabel, res)
    case Regex.timePerMove(res) => (timePerMoveLabel, res)
    case Regex.yourBot(res) => (yourBotLabel, res)
    case Regex.yourBotId(res) => (yourBotIdLabel, res)
  }
  val timeBankLabel = 'timeBank
  val timePerMoveLabel = 'timePerMove
  val yourBotLabel = 'yourBot
  val yourBotIdLabel = 'yourBotId
}
