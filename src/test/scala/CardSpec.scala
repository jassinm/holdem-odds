import org.scalatest.FunSpec
import org.scalatest.Matchers
import holdem._


class CardSpec extends FunSpec with Matchers {

  it("should be convetable to a bitwise representation") {
    val card = Card.from_string("Qs")
    card.bit_value() shouldEqual 67144223
    val bitvals = CardsParser.parse("2hQhAhAd8h").map(c=>c.bit_value())
    bitvals shouldEqual List(81922,67127839,268454953,268446761,4212241)

  }
  it("should be able to have a correct bitvalue") {
    val card = Card.from_string("Ad")
    val card1 = Card.from_string("2d")
    card.bit_value() should be > card1.bit_value()
  }
  it("bitvalues should be ordered (best for higher card)") {
    val bitvals = "23456789TQKA".map(x => Card.from_string(x.toString + "s").bit_value())
    val issorted = bitvals.view.zip(bitvals.tail).forall(x => x._1 <= x._2)
    issorted shouldEqual true
  }
}
