package holdem

import scala.util.Random

object Deck {

  def generate:Set[Card] = {
    val card_list = for {rank <- Rank.ranks; suit <- Suit.suits} yield(new Card(rank, suit))
    card_list.toSet
  }

  def shuffle(cards: Set[Card]):Set[Card] = {
    Random.shuffle(cards)
  }

  def apply():Set[Card] = {
    shuffle(generate)
  }

}
