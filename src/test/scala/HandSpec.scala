import org.scalatest.FunSpec
import org.scalatest.Matchers
import holdem._


class HandSpec extends FunSpec with Matchers {

  it("should understand straight flush") {

    val hand = Hand.from_string("2d3d4d5d6dQsJc")
    Hand.is_flush(hand) shouldEqual true
    val hand1 = Hand.from_string("Jc2d3d4d5d6dQs")
    Hand.is_flush(hand1) shouldEqual true
    val hand2 = Hand.from_string("Jc2d3d4d5dQs")
    Hand.is_flush(hand2) shouldEqual false

    Hand.handtype(hand) shouldEqual StraightFlush

  }

  it("should know how to compare same hand types"){

    val hand1 = Hand.from_string("AdAcAsAh5c")
    val hand2 = Hand.from_string("KdKcKsKh5c")
    Hand.is_four_of_kind(hand1) shouldEqual true
    Hand.is_four_of_kind(hand2) shouldEqual true

    //(hand1 > hand2) shouldEqual  true


  }

}
