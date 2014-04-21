object Def {

  type PlayerId = Int

  trait Strategy {
    def choose(hand: Card, draw: Card, state: State): StrategyResult
  }

  case class StrategyResult(
    discardType: DiscardType,
    target: Option[PlayerId]
  )

  trait DiscardType
  object DiscardType {
    case object Hand extends DiscardType
    case object Draw extends DiscardType
    case object System extends DiscardType
  }

  case class Discard(
    card: Card,
    discardType: DiscardType,
    playerId: PlayerId,
    target: Option[PlayerId]
  )

  case class Player(
    id: PlayerId,
    hand: Option[Card],
    discards: List[Discard]
  ) {
    val isPlaying: Boolean = hand.isDefined
    val isSafe: Boolean = discards match {
      case Discard(Handmaid, _, _, _) :: _ ⇒ true
      case _ ⇒ false
    }
    val isTargetable: Boolean = isPlaying && !isSafe
    val isNotCountess: Boolean = discards match {
      case Discard(Prince | King, _, _, _) :: _ ⇒ true
      case _ ⇒ false
    }
    val isTsumogiri: Boolean = discards match {
      case Discard(_, DiscardType.Draw, _, _) :: _ ⇒ true
      case _ ⇒ false
    }
  }

  case class State(
    deck: List[Card],
    players: List[Player],
    discards: List[Discard],
    burn: Card,
    previousState: Option[State]
  ) {
    val isFinished: Boolean = deck.isEmpty || players.count(_.isPlaying) <= 1
  }

  abstract class Card(
    val value: Int,
    val isRequiresTarget: Boolean
  )
  case object Guard    extends Card(1, true)
  case object Priest   extends Card(2, true)
  case object Baron    extends Card(3, true)
  case object Handmaid extends Card(4, false)
  case object Prince   extends Card(5, true)
  case object King     extends Card(6, true)
  case object Countess extends Card(7, false)
  case object Princess extends Card(8, false)

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

    val summary = state.previousState match {
      case Some(previousState) ⇒
        def prettyPrintCard(card: Option[Card]): String =
          card.fold("-")(_.toString)

        val playerPrevious = previousState.players.head
        val player = state.players.find(_.id == playerPrevious.id).get

        val handCard = playerPrevious.hand.get
        val drawnCard = previousState.deck.head
        val discard = state.discards.find(_.discardType != DiscardType.System).get
        val keptCard =
          if (handCard == discard.card)
            drawnCard
          else
            handCard

        val targetPrevious = discard.target flatMap (target ⇒ previousState.players find (_.id == target))
        val target = discard.target flatMap (target ⇒ state.players find (_.id == target))

        val playAction = s"[${player.id}] ($handCard + $drawnCard) discarding ${discard.card}"
        val targetAction = (target, targetPrevious) match {
          case (Some(target), Some(targetPrevious)) ⇒
            discard.card match {
              case Baron ⇒
                val targetValue = targetPrevious.hand map (_.value) get
                val comparator =
                  if (targetValue < keptCard.value)
                    "less than"
                  else if (targetValue > keptCard.value)
                    "greater than"
                  else
                    "equal to"
                s"\ntargeted [${target.id}] who had ${prettyPrintCard(targetPrevious.hand)} ($comparator my $keptCard)"
              case Prince ⇒
                s"\ntargeted [${target.id}] ${state.discards.head.card} -> ${prettyPrintCard(target.hand)}"
              case King ⇒
                s"\ntargeted [${target.id}] swapping ${prettyPrintCard(target.hand)} <-> ${prettyPrintCard(player.hand)}"
              case _ ⇒
                ""
            }
          case _ ⇒ ""
        }

        playAction + targetAction
      case None ⇒
        "-"
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

      s"""\n[${player.id}] ($prettyPrintedPlayerStatus)
         |Hand: $prettyPrintedPlayerHand
         |Discards: ${player.discards.map(prettyPrintPlayerDiscard).mkString}
         |""".stripMargin
    }

    val prettyPrintedCurrentState =
      s"""Deck: ${state.deck.mkString(" <- ")}
         |Burn: ${state.burn}
         |
         |Discards: ${state.discards.map(prettyPrintDiscard).mkString}
         |
         |Players: ${state.players.map(prettyPrintPlayer).mkString}""".stripMargin

    s"""$prettyPrintedLastStates
       |======================
       |
       |Summary:
       |$summary
       |
       |$prettyPrintedCurrentState""".stripMargin
  }
}
