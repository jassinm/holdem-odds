package holdem

object Holdem {
  def evaluate(hand: (Card, Card),
               community_cards: List[Card],
               other_players_cards: List[(Card, Card)]) : Int = {


    val myhandtype = Hand.handtype(community_cards ++ List(hand._1, hand._2))

    val other_cards = other_players_cards.map(c=> community_cards ++ List(c._1, c._2))

    val other_handtypes = other_cards.map(c => Hand.handtype(c))

    val best_other_hand = other_handtypes.sortBy(_.value).reverse.head

    // -1 lose 0 tie 1 win
    if (myhandtype.value > best_other_hand.value)
      1
    else if (myhandtype.value == best_other_hand.value)
      0
    else
      -1
  }

  def play_single_game(hand: (Card, Card),
                       community_cards: List[Card],
                       number_of_players: Int) = {

    val deck = Deck.generate

    val dealer_cards = (deck -- community_cards.toSet - hand._1 - hand._2)

    def play_game(cards: Set[Card]) = {

      val other_players = 1 to (number_of_players - 1)

      def gen_other_players_hands(cnt: Int, cards: List[Card],
                                  hands: List[(Card, Card)]): List[(Card, Card)] = cnt match{
        case 0 => hands
        case _ => val h1 = cards.head
          val cards1 = cards.tail
          val h2 = cards1.head
          val cards2 = cards1.tail
          gen_other_players_hands(cnt-1, cards2, hands ++ List((h1, h2)))
      }

      val other_players_hands = gen_other_players_hands(number_of_players - 1,
                                                        dealer_cards.toList,
                                                        List())
      val other_players_cards = other_players_hands.flatMap(t => List(t._1, t._2)).toSet
      val deck_cards = (dealer_cards -- other_players_cards).toList

      //make sure all five cards are dealt

      val (table_cards, _) = community_cards.size match {
        case 0 => //deal flop //turn and river
          (deck_cards.slice(0, 5), deck_cards.slice(6, deck_cards.size-1))
        case 3 => //deal turn and river
          (deck_cards.slice(0, 2), deck_cards.slice(6, deck_cards.size-1))
        case 4 => // deal river
          (community_cards ++ List(deck_cards.head), deck_cards.tail)
        case 5 =>
          (community_cards, deck_cards)
      }
      evaluate(hand, table_cards, other_players_hands)
    }

    play_game(Deck.shuffle(dealer_cards))


  }
  def calculate_odds(hand: (Card, Card),
                     community_cards: List[Card],
                     number_of_players: Int,
                     number_of_trials: Int = 10000) = {

    def run_montecarlo_trials(trials: Int, wins: Int, ties: Int): (Int, Int) = {

      val game_result = play_single_game(hand, community_cards, number_of_players)
      (trials, game_result) match {
        case (0, _) => (wins, ties)
        case (_, 1)=> run_montecarlo_trials(trials - 1, wins + 1, ties)
        case (_, 0) => run_montecarlo_trials(trials - 1, wins, ties + 1)
        case (_, _) => run_montecarlo_trials(trials - 1, wins, ties)
      }
    }

    val (wins, losses) = run_montecarlo_trials(number_of_trials, 0, 0)

    println(wins)
    (wins / number_of_trials.toFloat, losses / number_of_trials.toFloat)

  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    Holdem.play_single_game((Card.from_string("Ad"), Card.from_string("Ac")), List(Card.from_string("As"), Card.from_string("2d"), Card.from_string("3d")), 4)
    val (winprob, _) = Holdem.calculate_odds((Card.from_string("Ad"), Card.from_string("Ac")), List(Card.from_string("As"), Card.from_string("2d"), Card.from_string("3d")), 4)
    //val (winprob, _) = Holdem.calculate_odds((Card.from_string("Ad"), Card.from_string("Ac")), List(Card.from_string("As")), 4)
    println(winprob)

  }
}
