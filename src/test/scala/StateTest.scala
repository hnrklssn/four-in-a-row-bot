import org.scalatest.{FlatSpec, Matchers}
import model.State

/**
  * Created by henrik on 2017-08-16.
  */
class StateTest extends FlatSpec with Matchers {
  "The parse function" should "parse time bank, time per move, bot name and bot id" in {
    val exampleSettings = Seq(
      "settings timebank 10000",
      "settings time_per_move 500",
      "settings player_names player0,player1",
      "settings your_bot player0",
      "settings your_botid 0",
      "settings field_columns 7",
      "settings field_rows 6"
    )
    val resultSeq = Seq(
      State.timeBankLabel -> "10000",
      State.timePerMoveLabel -> "500",
      State.yourBotLabel -> "player0",
      State.yourBotIdLabel -> "0"
    )
    exampleSettings.collect(State.readSetting) should contain allElementsOf resultSeq
  }
}
