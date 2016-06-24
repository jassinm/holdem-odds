package holdem

import scopt.OptionParser


object Dealer {

  def evaluate(hand: List[Card],
               board: List[Card],
               other_players_hands: List[List[Card]]) : Int = {

    val player_hand = Hand(board ++ hand)
    val other_hands = other_players_hands.map(c=> Hand(board ++ c))
    val best_other_hand = other_hands.min

    player_hand compare best_other_hand

  }

}

case class Config(player_hand_str: String="",
                  community_cards_str: String="",
                  number_of_players: Int = 6)

object Holdem {


  def play_single_game(hand: List[Card],
                       community_cards: List[Card],
                       number_of_players: Int) = {

    val deck = Deck.generate

    val dealer_cards = (deck -- community_cards.toSet -- hand.toSet)

    def play_game(cards: Set[Card]) = {

      val other_players = 1 to (number_of_players - 1)

      def gen_other_players_hands(cnt: Int, cards: List[Card],
                                  hands: List[List[Card]]): List[List[Card]] = cnt match{
        case 0 => hands
        case _ => gen_other_players_hands(cnt-1, cards.drop(2), hands ++ List(cards.take(2)))
      }

      val other_players_hands = gen_other_players_hands(number_of_players - 1,
                                                        dealer_cards.toList,
                                                        List())
      val other_players_cards = other_players_hands.flatten.toSet
      val deck_cards = (dealer_cards -- other_players_cards).toList

      //make sure all five cards are dealt

      val (table_cards, _) = community_cards.size match {
        case 0 => //deal flop //turn and river
          (deck_cards.slice(0, 5), deck_cards.slice(6, deck_cards.size-1))
        case 3 => //deal turn and river
          (community_cards ++ deck_cards.slice(0, 2), deck_cards.slice(6, deck_cards.size-1))
        case 4 => // deal river
          (community_cards ++ List(deck_cards.head), deck_cards.tail)
        case 5 =>
          (community_cards, deck_cards)
      }
      Dealer.evaluate(hand, table_cards, other_players_hands)
    }

    play_game(Deck.shuffle(dealer_cards))


   }

  def calculate_odds(hand: List[Card],
                     community_cards: List[Card],
                     number_of_players: Int,
                     number_of_trials: Int = 10000): (Double, Double, Double) = {

    def run_montecarlo_trials(trials: Int, wins: Int, ties: Int, losses: Int): (Int, Int, Int) = {

      val game_result = play_single_game(hand, community_cards, number_of_players)
      (trials, game_result) match {
        case (0, _) => (wins, ties, losses)
        case (_, 1)=> run_montecarlo_trials(trials - 1, wins + 1, ties, losses)
        case (_, 0) => run_montecarlo_trials(trials - 1, wins, ties + 1, losses)
        case (_, -1) => run_montecarlo_trials(trials - 1, wins, ties, losses+1)
      }
    }

    val (wins, ties, losses) = run_montecarlo_trials(number_of_trials, 0, 0, 0)

    (wins / number_of_trials.toFloat,
     ties/ number_of_trials.toFloat,
     losses/ number_of_trials.toFloat
     )

  }

  def main(args: Array[String]): Unit = {

    def run(config: Config) = {
      (config.player_hand_str.size, config.community_cards_str.size) match  {
        case (4, 0) | (4, 6) | (4, 8) | (4, 10) =>
          val community_cards = Hand.from_string(config.community_cards_str)
          println("community cards: %s".format(community_cards.map(x=> x.toString).foldLeft("")((x,y)=> x+" "+y)))
          val player_hand = Hand.from_string(config.player_hand_str)
          println("player hand: %s".format(player_hand.map(x=> x.toString).reduceLeft((x,y)=> x+" "+y)))
          val (winprob, tieprob, lossprob) = Holdem.calculate_odds(player_hand, community_cards, config.number_of_players)

          println("win probability %s".format(winprob))
          println("tie probability %s".format(tieprob))
          println("loss probability %s".format(lossprob))
        case (_, _) =>
          println("validation failed")
      }
    }

    val parser = new OptionParser[Config]("holdem") {
      opt[Int]('n', "number_of_players").action( (x, c) =>
        c.copy(number_of_players = x) ).text("number of players")
      opt[String]('c', "community_cards").action( (x, c) =>
        c.copy(community_cards_str = x)).text("community cards: 3,4 or 5 cards")
      opt[String]('p', "player_hand").action( (x, c) =>
        c.copy(player_hand_str= x)).text("player cards: 2 cards")

      help("help").text("prints this usage text")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        run(config)
      case None =>
    }


  }
}
