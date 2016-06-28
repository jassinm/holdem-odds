package holdem

import scala.collection.immutable.ListMap


case class Hand(cards: List[Card])(
) extends Ordered[Hand]{

  def compare(that: Hand): Int = {
    Hand.evaluate(this.cards) compare Hand.evaluate(that.cards)
  }
}


object Hand{

  def from_string(s: String): List[Card] = {
    val pairs_s = s.sliding(2, 2).map(x => x.head.toString + x.tail.head.toString).toList
    pairs_s.map(x=> Card.from_string(x))

  }

  def combinations[T](k: Int, list: List[T]) : List[List[T]] =
    list match {
      case Nil => Nil
      case head :: xs =>
        if (k <= 0 || k > list.length) {
          Nil
        }
        else if (k == 1) {
          list.map(List(_))
        } else {
          combinations(k-1, xs).map(head :: _) ::: combinations(k, xs)
        }
   }

  private def find_fast(x: Int): Int ={
    var y: Long = x
    y += 0XE91AAA35L
    y ^= y >> 16
    y += y << 8
    y &= 0xffffffffL
    y ^= y >> 4
    val b = (y >> 8) & 0x1ff
    val a = (y + (y << 2)) >> 19
    val r = (a ^ LookupTable.hash_adjust(b.toInt)) & 0x1fff
    r.toInt
  }


  def evaluate(cards: List[Card]): Int = {
    def evaluate_5(cards: List[Card]): Int = {
      val List(c1, c2, c3, c4, c5) = cards.map(x=>x.bit_value())
      //chek for flushes and straight flushes
      val q = (c1 | c2 | c3 | c4 | c5) >> 16
      //check for straight and high card hands
      val s = LookupTable.unique5(q)
      val p = (c1 & 0xff) * (c2 & 0xff) * (c3 & 0xff) * (c4 & 0xff) * (c5 & 0xff)
      val flush = (c1 & c2 & c3 & c4 & c5 & 0xf000)
      (flush, s, p) match {
        case (0, 0, p) => LookupTable.hash_values(find_fast(p))
        case (0, s, _) => s
        case (flush, _, _) => LookupTable.flushes(q)
      }
    }

    def evaluate_6_or_7(cards: List[Card]): Int = {
      combinations(5, cards).map(x=> evaluate_5(x)).min
    }

    cards.size match {
      case 5 => evaluate_5(cards)
      case 6|7 => evaluate_6_or_7(cards)
    }
  }

  def is_flush(cards: List[Card]): Boolean ={
    (cards.map(x=> x.bit_value()).reduceLeft((x,y)=> x&y) & 0xf000) !=0
  }

}
