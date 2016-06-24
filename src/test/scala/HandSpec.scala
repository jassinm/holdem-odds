import org.scalatest.FunSpec
import org.scalatest.Matchers
import holdem._


class HandSpec extends FunSpec with Matchers {


  it("should be able to rank hands"){
    /*
     Hand strength is valued on a scale of 1 to 7462,
     where 1 is a Royal Flush and 7462 is unsuited 7-5-4-3-2
     */
    val royal_flush = CardsParser.parse("TsJsQsKsAs")
    Hand.evaluate(royal_flush) shouldEqual 1

    val straight_flush = CardsParser.parse("9sTsJsQsKs")
    Hand.evaluate(straight_flush) shouldEqual 2

    val straight_flush_queen_high = CardsParser.parse("8s9sTsJsQs")
    Hand.evaluate(straight_flush_queen_high) shouldEqual 3


    val seven_high= CardsParser.parse("7s6h4c3h2s")
    Hand.evaluate(seven_high) shouldEqual 7461
    val seven_high1 = CardsParser.parse("7s5h4c3h2s")
    Hand.evaluate(seven_high1) shouldEqual 7462

  }



}
