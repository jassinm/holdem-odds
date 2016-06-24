import org.scalatest.FunSpec
import org.scalatest.Matchers
import holdem._


class CardsParserSpec extends FunSpec with Matchers {

  it("should be able to parse an empty string") {
    CardsParser.parse("") shouldEqual List() 
  }
  it ("should be able to parse an invalid string") {
    CardsParser.parse("A") shouldEqual List() 
  }
  it ("should be able to parse a valid string") {
    CardsParser.parse("Ad") shouldEqual List(new Card(Ace(), Diamonds()))
    CardsParser.parse("Ad2s") shouldEqual List(new Card(Ace(), Diamonds()),
                                               new Card(Deuce(), Spades()))
  }
}
