object Def {

  type PlayerId = Int

  trait Strategy {
    def choose(hand: Card, draw: Card, state: State): DiscardType
  }

  trait DiscardType
  object DiscardType {
    case object Hand extends DiscardType
    case object Draw extends DiscardType
  }

  case class Discard(
    card: Card,
    discardType: DiscardType,
    playerId: PlayerId
  )

  case class Player(
    id: PlayerId,
    hand: Option[Card],
    discards: List[Discard]
  ) {
    val isPlaying: Boolean = hand.isDefined
    val isSafe: Boolean = discards match {
      case Discard(Handmaid, _, _) :: _ ⇒ true
      case _ ⇒ false
    }
    val isNotCountess: Boolean = discards match {
      case Discard(Prince | King, _, _) :: _ ⇒ true
      case _ ⇒ false
    }
    val isTsumogiri: Boolean = discards match {
      case Discard(_, DiscardType.Draw, _) :: _ ⇒ true
      case _ ⇒ false
    }
  }

  case class State(
    deck: List[Card],
    players: List[Player],
    discards: List[Discard],
    previousState: Option[State]
  ) {
    val isFinished: Boolean = deck.isEmpty
  }

  abstract class Card(val value: Int)
  case object Guard    extends Card(1)
  case object Priest   extends Card(2)
  case object Baron    extends Card(3)
  case object Handmaid extends Card(4)
  case object Prince   extends Card(5)
  case object King     extends Card(6)
  case object Countess extends Card(7)
  case object Princess extends Card(8)

  val ReferenceCards = List(
    List.fill(5)(Guard),
    List.fill(2)(Priest),
    List.fill(2)(Baron),
    List.fill(2)(Handmaid),
    List.fill(2)(Prince),
    List.fill(1)(King),
    List.fill(1)(Countess),
    List.fill(1)(Princess)
  ).flatten

  def prettyPrint(state: State, printPrevious: Boolean = false): String = {
    val prettyPrintedLastStates = state.previousState match {
      case Some(previousState) if printPrevious ⇒
        prettyPrint(previousState, printPrevious)
      case _ ⇒ ""
    }

    def prettyPrintDiscard(discard: Discard) =
      s"\n${discard.playerId}: ${discard.card} (${discard.discardType})"

    def prettyPrintPlayer(player: Player) = {
      val prettyPrintedPlayerStatus = player.isPlaying match {
        case true ⇒ "playing"
        case false ⇒ "out"
      }

      val prettyPrintedPlayerHand = player.hand match {
        case Some(card) ⇒ s"$card (${card.value})"
        case _ ⇒ "-"
      }

      def prettyPrintPlayerDiscard(discard: Discard) =
        s"\n- ${discard.card} (${discard.discardType})"

      s"""\n[${player.id}] - $prettyPrintedPlayerStatus
         |Hand: $prettyPrintedPlayerHand
         |Discards: ${player.discards.map(prettyPrintPlayerDiscard).mkString}
         |""".stripMargin
    }

    val prettyPrintedCurrentState =
      s"""Deck: ${state.deck.mkString(" ← ")}
         |
         |Discards: ${state.discards.map(prettyPrintDiscard).mkString}
         |
         |Players: ${state.players.map(prettyPrintPlayer).mkString}""".stripMargin

    s"""$prettyPrintedLastStates
       |======================
       |
       |$prettyPrintedCurrentState""".stripMargin
  }
}
