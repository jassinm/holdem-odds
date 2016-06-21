package holdem

abstract sealed class Rank extends Ordered[Rank]{
  val value: Int
  val prime: Int
  override def toString = {
    value match {
      case 8 => "T"
      case 9 => "J"
      case 10 => "Q"
      case 11 => "K"
      case 12 => "A"
      case _ =>  "%s".format(value+2)
    }
  }

  def compare(that: Rank) = value compare that.value
}

case class Two() extends Rank {val value=0; val prime=2}
case class Three() extends Rank{val value=1; val prime=3}
case class Four() extends Rank{val value=2; val prime=5}
case class Five() extends Rank{val value=3; val prime=7}
case class Six() extends Rank{val value=4; val prime=11}
case class Seven() extends Rank{val value=5; val prime=13}
case class Eight() extends Rank{val value=6; val prime=17}
case class Nine() extends Rank{val value=7; val prime=19}
case class Ten() extends Rank{val value=8; val prime=23}
case class Jack() extends Rank{val value=9; val prime=29}
case class Queen() extends Rank{val value=10; val prime=31}
case class King() extends Rank{val value=11; val prime=37}
case class Ace() extends Rank{val value=12; val prime=41}

object Rank {

  def from_char(c: Char): Rank = c match {
    case '2' => new Two()
    case '3' => new Three()
    case '4' => new Four()
    case '5' => new Five()
    case '6' => new Six()
    case '7' => new Seven()
    case '8' => new Eight()
    case '9' => new Nine()
    case 'T' => new Ten()
    case 'J' => new Jack()
    case 'j' => new Jack()
    case 'Q' => new Queen()
    case 'q' => new Queen()
    case 'K' => new King()
    case 'k' => new King()
    case 'A' => new Ace()
    case 'a' => new Ace()
  }

  val ranks: Array[Rank] = {
    val ranks_str: Array[Char] = "23456789TJQKA".toArray
    ranks_str.map(x=>Rank.from_char(x))
  }
}



sealed trait Suit

case class Hearts() extends Suit {
  override def toString = {
       "♡"
    }
  }
case class Diamonds() extends Suit{
  override def toString = {
    "♢"
  }
}
case class Clubs() extends Suit {
  override def toString = {
    "♧"
  }
}
case class Spades() extends Suit{
  override def toString = {
    "♤"
  }
}


object Suit{
  def from_char(c: Char): Suit = c match{
    case 'h' => Hearts()
    case '♥' => Hearts()
    case '♡' => Hearts()
    case 'd' => Diamonds()
    case '♦' => Diamonds()
    case '♢' => Diamonds()
    case 'c' => Clubs()
    case '♣' => Clubs()
    case '♧' => Clubs()
    case 's' => Spades()
    case '♠' => Spades()
    case '♤' => Spades()
  }
  val suits: Array[Suit] = Array(Hearts(), Diamonds(), Clubs(), Spades())


}

class Card(r:Rank, s:Suit) {
  val rank = r
  val suit = s

  override def equals(o: Any) = o match {
    case that: Card => this.rank == that.rank && this.suit == that.suit
    case _ => false
  }

  override def toString = {
    "Card( %s%s )".format(r, s)
  }
}

object Card{

  def from_string(s: String): Card = {
    new Card(Rank.from_char(s(0)), Suit.from_char(s(1)))
  }

}
