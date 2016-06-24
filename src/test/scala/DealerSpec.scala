
import org.scalatest.{Matchers, FunSpec}
import holdem._


class DealerSpec extends FunSpec with Matchers {

  it("should be able to evaluate who the winner is") {
    val hand = CardsParser.parse("2d2s")
    val community_cards = CardsParser.parse("2c3d4h")
    val other_players_cards = List(CardsParser.parse("3s4s"))
    Dealer.evaluate(hand, community_cards, other_players_cards) shouldEqual 1
  }
}
