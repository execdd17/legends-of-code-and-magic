package codinggame

import scala.collection.mutable.ListBuffer

case class CardAbilities(abilities: String) {
  // Charge: Creatures with Charge can attack the turn they are summoned.
  def hasCharge: Boolean = abilities.toUpperCase().contains('C')

  // Guard: Enemy creatures must attack creatures with Guard first.
  def hasGuard: Boolean = abilities.toUpperCase().contains('G')

  // Breakthrough: Creatures with Breakthrough can deal extra damage to the opponent when they
  // attack enemy creatures. If their attack damage is greater than the defending creature's defense,
  // the excess damage is dealt to the opponent.
  def hasBreakthrough: Boolean = abilities.toUpperCase().contains('B')
}

object Card {
  final val IN_MY_HAND = 0
  final val ON_MY_SIDE = 1
  final val ON_THEIR_SIDE = -1
}

case class Card(cardnumber: Int,
                instanceid: Int,
                location: Int,
                cardtype:Int,
                cost: Int,
                attack:Int,
                defense:Int,
                abilities:CardAbilities) {

  def inMyHand: Boolean = location == Card.IN_MY_HAND
  def onMySideOfBoard: Boolean = location == Card.ON_MY_SIDE
  def onEnemySideOfBoard: Boolean = location == Card.ON_THEIR_SIDE
}

object CardManager {
  def compileCreatureToCreatureAttack(attacker: Card, victim: Card): String = {
    s"ATTACK ${attacker.instanceid} ${victim.instanceid}"
  }

  def compileCreatureToFaceAttack(attacker: Card): String = {
    s"ATTACK ${attacker.instanceid} -1"
  }

  def compileSummonAction(card: Card): String = {
    s"SUMMON ${card.instanceid}"
  }

  def compilePassAction: String = "PASS"
}

class CardManager(cards: List[Card]) {
  val summonedEnemyCards:List[Card] = cards.filter(card => card.onEnemySideOfBoard)
  val cardsInMyHand:List[Card] = cards.filter(card => card.inMyHand)
  val mySummonedCards:List[Card] = cards.filter(card => card.onMySideOfBoard)
  val mySummonsReadyToCharge:ListBuffer[Card] = ListBuffer.empty[Card]

  def getActionsForTurn(currentMana: Int): String = {
    val summoningActions = getSummoningActionsForTurn(currentMana)
    val attackActions = getAttackActionsForTurn
    Console.err.println(s"summoning actions: $summoningActions")
    Console.err.println(s"attack actions: $attackActions")

    if (summoningActions.isEmpty && attackActions.isEmpty) {
      CardManager.compilePassAction
    } else {
      List(summoningActions, attackActions).filter(_.nonEmpty).mkString(";")
    }
  }

  private def getAttackActionsForTurn: String = {
    if (mySummonedCards.isEmpty && mySummonsReadyToCharge.isEmpty) {
      Console.err.println("I have no creatures able to attack right now")
      return ""
    }

    val allAvailableCards = mySummonedCards ::: mySummonsReadyToCharge.toList
    val enemyGuardCards = getEnemyGuardCards

    // TODO: handle multiple enemy guard cards on their side
    if (enemyGuardCards.nonEmpty) {
      val (guardKillers, weaklings) = allAvailableCards.partition(_.attack >= enemyGuardCards.head.defense)
      if (guardKillers.isEmpty) {
        // have the weaklings group up to hopefully kill guard
        weaklings.map(card => CardManager.compileCreatureToCreatureAttack(card, enemyGuardCards.head)).mkString(";")
      } else {
        // take the first creature who can kill the guard, and use everyone else to attack the face
        List(
          List(CardManager.compileCreatureToCreatureAttack(guardKillers.head, enemyGuardCards.head)),
          guardKillers.tail.map(card => CardManager.compileCreatureToFaceAttack(card)),
          weaklings.map(card => CardManager.compileCreatureToFaceAttack(card))
        ).flatten.mkString(";")
      }
    } else {
      allAvailableCards.map(card => CardManager.compileCreatureToFaceAttack(card)).mkString(";")
    }
  }

  private def getSummoningActionsForTurn(currentMana: Int): String = {
    val cardsToSummon = getMostExpensiveCardsICanAfford(currentMana)
    mySummonsReadyToCharge ++= cardsToSummon.filter(_.abilities.hasCharge)
    cardsToSummon.map(card => CardManager.compileSummonAction(card)).mkString(";")
  }

  private def getEnemyGuardCards: List[Card] = summonedEnemyCards.filter(card => card.abilities.hasGuard)

  // TODO: Convert this to be based on mana efficiency. E.g. favor using as much mana as possible per turn
  private def getMostExpensiveCardsICanAfford(totalMana: Int): List[Card] = {
    val highestToLowestCostCards = cardsInMyHand.sortBy(- _.cost)
    var currentMana = totalMana
    val cardsToKeep = ListBuffer.empty[Card]

    highestToLowestCostCards.foreach { card =>
      if (currentMana - card.cost >= 0) {
        currentMana -= card.cost
        cardsToKeep += card
      }
    }

    cardsToKeep.toList
  }

}
