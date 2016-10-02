import cats.kernel.Monoid
import cats.implicits._

case class Statistic(truePositives: Int, falsePositives: Int, falseNegatives: Int) {
  val recall = truePositives.toDouble / (truePositives + falseNegatives)
  val precision = truePositives.toDouble / (truePositives + falsePositives)
  val fMeasure = 2 * precision * recall / (precision + recall)
}

object Statistic {
  implicit val statsMonoid = new Monoid[Statistic] {
    override def empty: Statistic = Statistic(0,0,0)
    override def combine(sta1: Statistic, sta2: Statistic): Statistic = Statistic(
      sta1.truePositives + sta2.truePositives,
      sta1.falsePositives + sta2.falsePositives,
      sta1.falseNegatives + sta2.falseNegatives
    )
  }
}

val statistics = List(
  Statistic(1,2,3),
  Statistic(3,2,1),
  Statistic(5,5,5)
)

statistics.foldLeft(Statistic(0,0,0)){ case (sta1, sta2) => Statistic(
  sta1.truePositives + sta2.truePositives,
  sta1.falsePositives + sta2.falsePositives,
  sta1.falseNegatives + sta2.falseNegatives
)}


// this is what we gain
// make implicit explicit :)

statistics.
  foldLeft(Monoid[Statistic].empty)(Monoid[Statistic].combine)

Monoid[Statistic]
  .combineAll(statistics)

// would be even cooler to just...
statistics
  .combineAll