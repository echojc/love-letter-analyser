import scala.util.Random
import Def._

object Impl {

  def run(playerCount: Int, strategy: Strategy): State = {
    def iter(state: State): State =
      if (state.isFinished)
        state
      else
        iter(advanceState(strategy)(state))
    iter(newGame(playerCount))
  }

  def newGame(playerCount: Int): State = {
    val shuffledCards = Random.shuffle(ReferenceCards.toList)
    val (playerCards, deck) = shuffledCards.splitAt(playerCount)
    val playerIds = 1 to playerCount
    val players = (playerCards zip playerIds) map {
      case (card, id) ⇒ Player(id, Some(card), Nil)
    }
    State(deck, players, Nil, None)
  }

  def advanceState(strategy: Strategy)(state: State): State = {
    val drawnCard :: remainingDeck = state.deck
    val player = state.players.head
    val handCard = player.hand.get // TODO

    val discardType = strategy.choose(handCard, drawnCard, state)
    val (discardedCard, keptCard) = discardType match {
      case DiscardType.Hand ⇒ (handCard, drawnCard)
      case DiscardType.Draw ⇒ (drawnCard, handCard)
    }
    val discard = Discard(discardedCard, discardType, player.id)

    val updatedPlayer = (discardedCard, keptCard) match {
      //case (Princess, _) ⇒
      //  player.copy(
      //    hand = keptCard,
      //    discards = discard :: player.discards,
      //    isPlaying = false
      //  )
      //case (Prince | King, Countess) ⇒
      //  player.copy(
      //    hand = keptCard,
      //    discards = discard :: player.discards,
      //    isPlaying = false
      //  )
      case (discarded @ _, kept @ _) ⇒
        println(s"### unhandled: discarded $discarded, kept $kept")
        player.copy(
          hand = Some(keptCard),
          discards = discard :: player.discards
        )
    }

    State(
      remainingDeck,
      cyclePlayers(updatedPlayer :: state.players.tail),
      discard :: state.discards,
      Some(state)
    )
  }

  def cyclePlayers(players: List[Player]): List[Player] = {
    val currentPlayer :: otherPlayers = players
    val (skippedPlayers, rest) = otherPlayers.span(!_.isPlaying)
    rest ++ (currentPlayer :: skippedPlayers)
  }
}
