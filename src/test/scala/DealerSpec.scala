
import org.scalatest.{Matchers, FunSpec}
import holdem._


class DealerSpec extends FunSpec with Matchers {

  it("should be able to evaluate who the winner is") {
    val hand = CardsParser.parse("2dAs")
    val community_cards = CardsParser.parse("2c3d4h")
    val other_players_cards = List(CardsParser.parse("3s4s"))
    Dealer.evaluate(hand, community_cards, other_players_cards) shouldEqual -1

    val hand1 = CardsParser.parse("AsKs")
    val community_cards1 = CardsParser.parse("AdKcKh")
    val other_players_cards1 = List(CardsParser.parse("6sQd"))

    Dealer.evaluate(hand1, community_cards1, other_players_cards1) shouldEqual 1
  }
}
