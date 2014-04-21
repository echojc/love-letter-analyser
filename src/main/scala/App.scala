import scala.util.Random
import Def._
import Impl._

object App extends App {

  object RandomStrategy extends Strategy {
    override def choose(hand: Card, draw: Card, state: State): DiscardType =
      Random.shuffle(List(DiscardType.Hand, DiscardType.Draw)).head
  }

  println(prettyPrint(run(4, RandomStrategy), printPrevious = true))
}
