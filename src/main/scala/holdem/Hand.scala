package holdem

import scala.collection.immutable.ListMap


sealed trait Hand extends Ordered[Hand]{
  val value: Int
  val rank: Int
  def compare(that: Hand): Int  = {
    val comp = value compare that.value
    comp match {
      case 0 => comp
      case _ => comp

    }
  }
}
case object RoyalStraightFlush extends Hand{val value=21; val rank=10}
case class StraightFlush(ivalue: Int) extends Hand{val value=ivalue*rank; val rank=9}
case object FourOfKind extends Hand{val value=19; val rank=8}
case object FullHouse extends Hand{val value=18; val rank=7}
case object Flush extends Hand{val value=17; val rank=6}
case object Straight extends Hand{val value=16; val rank=5}
case object ThreeOfKind extends Hand{val value=15; val rank=4}
case object TwoPair extends Hand{val value=14; val rank=3}
case object OnePair extends Hand{val value=13; val rank=2}
case class HighCard(ivalue: Int) extends Hand{val value=ivalue; val rank=1}

object Hand{

  def from_string(s: String): List[Card] = {
    val pairs_s = s.sliding(2, 2).map(x => x.head.toString + x.tail.head.toString).toList
    pairs_s.map(x=> Card.from_string(x))

  }


  def check_fun(fun: List[Card]=> Boolean, cards: List[Card]) : Boolean = {
    cards.length match {
      case 5 => fun(cards)
      case 6 => fun(cards) | fun(cards.tail)
      case 7 => fun(cards) | fun(cards.tail) | fun(cards.tail.tail)
      case _ => false
    }
  }

  def is_straight(cards: List[Card]) : Boolean = {
    def are_consecutive(card1: Card, card2: Card) : Boolean = {

      (card1.rank.value, card2.rank.value) match {
        case (12, 0) => true
        case _ => card2.rank.value == card1.rank.value + 1
      }
    }
    def check_is_straight(cards: List[Card]) : Boolean = {
      cards match {
        case x :: y :: tail => are_consecutive(x, y) & check_is_straight(tail)
        case x => true
      }
    }
    check_fun(check_is_straight, cards.sortBy(_.rank))
  }

  def is_flush(cards: List[Card]) : Boolean = {
    def check_is_flush(cards: List[Card]) : Boolean = {
      val suit_count = cards.map(c=>(c.suit,c)).groupBy(_._1).mapValues(_.length)
      suit_count.values.filter(v=> v==5).isEmpty == false

    }
    check_fun(check_is_flush, cards)
  }

  def is_straight_flush(cards: List[Card]) : Boolean = {
    is_straight(cards) & is_flush(cards)
  }

  def is_four_of_kind(cards: List[Card]) : Boolean = {
    val rank_count = cards.map(c=> (c.rank, c)).groupBy(_._1).mapValues(_.length)
    rank_count.values.filter(v=> v==4).toList.length == 1
  }

  def is_full_house(cards: List[Card]) : Boolean = {
    val rank_count = cards.map(c=> (c.rank, c)).groupBy(_._1).mapValues(_.length)
    rank_count.values.filter(v=> v==3 & v==2).toList.length == 2
  }

  def is_three_of_kind(cards: List[Card]) : Boolean = {
    val rank_count = cards.map(c=> (c.rank, c)).groupBy(_._1).mapValues(_.length)
    rank_count.values.filter(v=> v==3).toList.length == 1
  }

  def is_two_pair(cards: List[Card]) : Boolean = {
    val rank_count = cards.map(c=> (c.rank, c)).groupBy(_._1).mapValues(_.length)
    rank_count.values.filter(v=> v==2).toList.length == 2
  }

  def is_one_pair(cards: List[Card]) : Boolean = {
    val rank_count = cards.map(c=> (c.rank, c)).groupBy(_._1).mapValues(_.length)
    rank_count.values.filter(v=> v==2).toList.length == 1
  }

  val type_check_fun = ListMap(StraightFlush -> is_straight_flush _,
                               FourOfKind -> is_four_of_kind _,
                               FullHouse -> is_full_house _,
                               Flush -> is_flush _,
                               Straight -> is_straight _,
                               ThreeOfKind -> is_three_of_kind _,
                               TwoPair -> is_two_pair _,
                               OnePair -> is_one_pair _)


  def handtype(cards: List[Card]) : Hand= {

    if (is_straight_flush(cards))
      StraightFlush(cards.sortBy(_.rank).reverse.head.rank.value)
    else if (is_four_of_kind(cards))
      FourOfKind
    else if (is_full_house(cards))
      FullHouse
    else if (is_flush(cards))
      Flush
    else if (is_straight(cards))
      Straight
    else if (is_three_of_kind(cards))
      ThreeOfKind
    else if (is_two_pair(cards))
      TwoPair
    else if (is_one_pair(cards) == true)
      OnePair
    else
      HighCard(cards.sortBy(_.rank).reverse.head.rank.value)
  }
}
