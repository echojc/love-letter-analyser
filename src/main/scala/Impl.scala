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
    val burn :: shuffledCards = Random.shuffle(ReferenceCards.toList)
    val (playerCards, deck) = shuffledCards.splitAt(playerCount)
    val playerIds = 1 to playerCount
    val players = (playerCards zip playerIds) map {
      case (card, id) ⇒ Player(id, Some(card), Nil)
    }
    State(deck, players, Nil, burn, None)
  }

  def advanceState(strategy: Strategy)(state: State): State = {
    val drawnCard :: remainingDeck = state.deck
    val currentPlayer = state.players.head
    val handCard = currentPlayer.hand.get

    val cardsInPlay = Set(handCard, drawnCard)
    val strategyResult =
      if (cardsInPlay == Set(Countess, Prince) || cardsInPlay == Set(Countess, King)) {
        println(s"### enforcing Countess discard!")
        val discardType =
          if (handCard == Countess)
            DiscardType.Hand
          else
            DiscardType.Draw
        StrategyResult(discardType, None)
      } else {
        strategy.choose(handCard, drawnCard, state)
      }
    val discardType = strategyResult.discardType

    val (discardedCard, keptCard) = discardType match {
      case DiscardType.Hand ⇒ (handCard, drawnCard)
      case DiscardType.Draw ⇒ (drawnCard, handCard)
    }
    val target = discardedCard.isRequiresTarget match {
      case true  ⇒ strategyResult.target
      case false ⇒ None
    }
    val targetPlayer = target flatMap { target ⇒
      state.players.tail find (p ⇒ p.isTargetable && p.id == target)
    }
    val discard = Discard(discardedCard, discardType, currentPlayer.id, target)
    val updatedPlayer = currentPlayer.copy(
      hand = Some(keptCard),
      discards = discard :: currentPlayer.discards
    )

    val defaultState =
      State(
        remainingDeck,
        cyclePlayers(updatedPlayer :: state.players.tail),
        discard :: state.discards,
        state.burn,
        Some(state)
      )

    (discardedCard, keptCard) match {
      case (Princess, _) ⇒
        println(s"### discarded Princess!")
        val otherDiscard = Discard(keptCard, DiscardType.System, currentPlayer.id, None)
        val reupdatedPlayer = updatedPlayer.copy(
          hand = None,
          discards = otherDiscard :: updatedPlayer.discards
        )
        defaultState.copy(
          players = cyclePlayers(reupdatedPlayer :: state.players.tail),
          discards = otherDiscard :: defaultState.discards
        )

      case (discarded @ (Prince | King), Countess) ⇒
        println(s"### discarded $discarded when holding Countess!")
        val otherDiscard = Discard(keptCard, DiscardType.System, currentPlayer.id, None)
        val reupdatedPlayer = updatedPlayer.copy(
          hand = None,
          discards = otherDiscard :: updatedPlayer.discards
        )
        defaultState.copy(
          players = cyclePlayers(reupdatedPlayer :: state.players.tail),
          discards = otherDiscard :: defaultState.discards
        )

      case (Baron, _) ⇒
        println(s"### discarded Baron targeting ${target.fold("-")(id ⇒ s"[$id]")} (valid: ${targetPlayer.isDefined})")
        targetPlayer match {
          case Some(targetPlayer) ⇒
            val currentPlayerCard = updatedPlayer.hand.get
            val targetPlayerCard = targetPlayer.hand.get

            if (currentPlayerCard.value < targetPlayerCard.value) {
              val otherDiscard = Discard(keptCard, DiscardType.System, currentPlayer.id, None)
              val reupdatedPlayer = updatedPlayer.copy(
                hand = None,
                discards = otherDiscard :: updatedPlayer.discards
              )
              defaultState.copy(
                players = cyclePlayers(reupdatedPlayer :: state.players.tail),
                discards = otherDiscard :: defaultState.discards
              )
            } else if (currentPlayerCard.value > targetPlayerCard.value) {
              val otherDiscard = Discard(targetPlayerCard, DiscardType.System, targetPlayer.id, None)
              val otherPlayers = state.players.tail.updated(
                index = state.players.tail.indexWhere(_ == targetPlayer),
                elem = targetPlayer.copy(
                  hand = None,
                  discards = otherDiscard :: targetPlayer.discards
                )
              )
              defaultState.copy(
                players = cyclePlayers(updatedPlayer :: otherPlayers),
                discards = otherDiscard :: defaultState.discards
              )
            } else {
              defaultState
            }
          case _ ⇒
            defaultState
        }

      case (Handmaid, kept @ _) ⇒
        println(s"### discarded Handmaid (keeping $kept)")
        defaultState

      case (Prince, _) ⇒
        println(s"### discarded Prince targeting ${target.fold("-")(id ⇒ s"[$id]")} (valid: ${targetPlayer.isDefined})")
        targetPlayer match {
          case Some(targetPlayer) ⇒
            val targetDiscard = Discard(targetPlayer.hand.get, DiscardType.System, targetPlayer.id, None)
            val otherPlayers = state.players.tail.updated(
              index = state.players.tail.indexWhere(_ == targetPlayer),
              elem = targetPlayer.copy(
                hand = remainingDeck.headOption,
                discards = targetDiscard :: targetPlayer.discards
              )
            )
            defaultState.copy(
              deck = defaultState.deck.drop(1),
              players = cyclePlayers(updatedPlayer :: otherPlayers),
              discards = targetDiscard :: defaultState.discards
            )
          case _ ⇒
            defaultState
        }

      case (King, _) ⇒
        println(s"### discarded King targeting ${target.fold("-")(id ⇒ s"[$id]")} (valid: ${targetPlayer.isDefined})")
        targetPlayer match {
          case Some(targetPlayer) ⇒
            val reupdatedPlayer = updatedPlayer.copy(
              hand = targetPlayer.hand
            )
            val otherPlayers = state.players.tail.updated(
              index = state.players.tail.indexWhere(_ == targetPlayer),
              elem = targetPlayer.copy(
                hand = updatedPlayer.hand
              )
            )
            defaultState.copy(
              players = cyclePlayers(reupdatedPlayer :: otherPlayers)
            )
          case _ ⇒
            defaultState
        }

      case (Countess, kept @ _) ⇒
        println(s"### discarded Countess (keeping $kept)")
        defaultState

      case (discarded @ _, kept @ _) ⇒
        println(s"### unhandled: discarded $discarded targeting ${target.fold("-")(id ⇒ s"[$id]")}, kept $kept")
        defaultState
    }
  }

  def cyclePlayers(players: List[Player]): List[Player] = {
    val currentPlayer :: otherPlayers = players
    val (skippedPlayers, rest) = otherPlayers.span(!_.isPlaying)
    rest ++ (currentPlayer :: skippedPlayers)
  }
}
