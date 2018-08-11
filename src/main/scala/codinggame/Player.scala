import codinggame._

import scala.collection.mutable.ListBuffer

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {

  val cardDrafter = new CardDrafter(DraftingStratagem.ZooStrategy)

  // game loop
  while(true) {

    var Array(playerhealth, playermana, playerdeck, playerrune) = for(i <- readLine split " ") yield i.toInt
    val Array(enemyhealth, enemymana, enemydeck, enemyrune) = for(i <- readLine split " ") yield i.toInt

    val opponenthand = readInt
    val cardcount = readInt
    val cards = ListBuffer.empty[Card]

    for(i <- 0 until cardcount) {
      val Array(_cardnumber, _instanceid, _location, _cardtype, _cost, _attack, _defense, abilities, _myhealthchange, _opponenthealthchange, _carddraw) = readLine split " "
      val cardnumber = _cardnumber.toInt
      val instanceid = _instanceid.toInt
      val location = _location.toInt
      val cardtype = _cardtype.toInt
      val cost = _cost.toInt
      val attack = _attack.toInt
      val defense = _defense.toInt
      val myhealthchange = _myhealthchange.toInt
      val opponenthealthchange = _opponenthealthchange.toInt
      val carddraw = _carddraw.toInt

      cards += Card(cardnumber, instanceid, location, cardtype, cost, attack, defense, CardAbilities(abilities))
    }

    // DRAFT PHASE
    if (playermana == 0) {
      val choice = cardDrafter.chooseCard(cards: _*)

      if (choice == cards(0))
        println("PICK 0")
      else if (choice == cards(1))
        println("PICK 1")
      else if (choice == cards(2))
        println("PICK 2")
      else
        println("PASS")

      // Console.err.println("I would have choose " + choice)

      // val creatureCardIndex = cards.indexWhere(card => card.cardtype == 0)
      // if (creatureCardIndex >= 0) {
      //     println("PICK " + creatureCardIndex)
      // } else {
      //     println("PASS")
      // }
    } else {
      val cardManager = new CardManager(cards.toList)
      val actions = cardManager.getActionsForTurn(playermana)
      println(actions)
    }

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
  }
}