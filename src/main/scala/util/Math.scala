package util

object Math {
  private val rand = scala.util.Random
  def weightedRandomPick[T <: (_, Double)](weightedXs: Seq[T]): T = {
    val weightSum = weightedXs.foldLeft(0.0)((acc, tup) => acc + tup._2)
    val cumPickWeight = rand.nextDouble() * weightSum
    val temp = weightedXs.sortBy(-_._2)
      .foldLeft[Either[Double, T]](Left(0.0)){
        case (Right(pick), _) => Right(pick)
        case (Left(cum), x) => if(cum + x._2 >= cumPickWeight) {
          Right(x)
        } else {
          Left(cum + x._2)
        }
      }
    /*println(weightedXs)
    println(s"EitherVal: ${temp.fold(_.toString, _.toString())}")
    println(s"Is Right: ${temp.isRight} (${temp.right.getOrElse('@')})")
    println(s"cumPickWeight: $cumPickWeight\tweightSum: $weightSum")*/
      temp.right.get
  }

  def dampenedSinusoidal(decay: Double, period: Double): Double => Double = { (x: Double) =>
    (math.sin(x / period) + 1) * 0.5 * math.exp(-decay*x)
  }
}
