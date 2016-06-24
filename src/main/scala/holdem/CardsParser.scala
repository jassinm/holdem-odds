package holdem


object CardsParser{

  def parse(cards_str: String): List[Card] = {

    def parse_pair(cards_str: String) : List[Card] = {
      val pairs_s = cards_str.sliding(2, 2)
      .map(x => x.head.toString + x.tail.head.toString)
      .toList
      val cards = pairs_s.map(x=> Card.from_string(x))
      cards
    }
    cards_str.matches("([aAKkQqJjTt|0-9][sSdDhHcC]){1,}") match {
      case false => List()
      case true => parse_pair(cards_str)
    }
  }

}
