import com.hnrklssn.fourinarow.learning.{H2Persistence, NeuralBoardRater, Persistence}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by henrik on 2017-08-23.
  */
class PersistenceTest extends FlatSpec with Matchers {
  "H2Persistence" should "store match results" in {
    H2Persistence.initialize()
    val h2: Persistence = new H2Persistence(0)
    val bot1 = NeuralBoardRater(1)
    val bot2 = NeuralBoardRater(2)
    h2.recordMatchResult(bot1, bot2, Some(bot1 -> true))
    h2.recordMatchResult(bot1, bot2, Some(bot2 -> false))
    h2.recordMatchResult(bot2, bot1, Some(bot2 -> true))
    h2.recordMatchResult(bot2, bot1, None)
    assert(h2.matchStats(bot1) == (1, 1, 2))
    assert(h2.matchStats(bot2) == (2, 1, 1))
  }

  "Write to file" should "write to file" in {
    val fileName = "test"
    val path = java.io.File.createTempFile(fileName, null).getAbsolutePath
    val bot = NeuralBoardRater(0)
    bot.writeToFile(path)
    val same = NeuralBoardRater.fromFile(path, explore = true)
    assert(bot.id == same.id)
    assert(bot.version == same.version)
    assert(bot == same)
  }

  it should "not require that the file already exists" in {
    val path = "test.weights"
    val bot = NeuralBoardRater(0)
    bot.writeToFile(path)
    val same = NeuralBoardRater.fromFile(path, explore = true)
    assert(bot.id == same.id)
    assert(bot.version == same.version)
    assert(bot == same)
  }
}
