import scala.util.Random
import Def._
import Impl._

object App extends App {

  object RandomStrategy extends Strategy {
    def oneOf[T](xs: List[T]): Option[T] =
      Random.shuffle(xs).headOption

    override def choose(hand: Card, draw: Card, state: State): StrategyResult = {
      val discardType = oneOf(List(DiscardType.Hand, DiscardType.Draw)).get
      val discardCard = discardType match {
        case DiscardType.Hand ⇒ hand
        case DiscardType.Draw ⇒ draw
      }
      val target =
        if (discardCard.isRequiresTarget)
          oneOf(state.players.tail.filter(_.isTargetable)) map (_.id)
        else
          None
      StrategyResult(discardType, target)
    }
  }

  println(prettyPrint(run(4, RandomStrategy), printPrevious = true))
}
