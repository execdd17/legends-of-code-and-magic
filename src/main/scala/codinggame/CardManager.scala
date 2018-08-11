package codinggame

import codinggame.DraftingStratagem.CardType

import scala.Predef.augmentString
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Action {
  def getActionName: String
  def combineActions(actions:Action)
}

//trait TargetedAction extends Action {
//  def getTargetId: String
//}
//
//trait
//
//object PassAction extends Action {
//  override def getActionName: String = "PASS"
//}

case class CardAbilities(abilities: String) {
  // Charge: Creatures with Charge can attack the turn they are summoned.
  def hasCharge: Boolean = abilities.toUpperCase().contains('C')

  // Guard: Enemy creatures must attack creatures with Guard first.
  def hasGuard: Boolean = abilities.toUpperCase().contains('G')

  def hasLethal: Boolean = abilities.toUpperCase().contains('L')

  def hasDrain: Boolean = abilities.toUpperCase().contains('D')

  def hasWard: Boolean = abilities.toUpperCase().contains('W')

  // Breakthrough: Creatures with Breakthrough can deal extra damage to the opponent when they
  // attack enemy creatures. If their attack damage is greater than the defending creature's defense,
  // the excess damage is dealt to the opponent.
  def hasBreakthrough: Boolean = abilities.toUpperCase().contains('B')

  def hasAbilities: Boolean = abilities != "------"
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

  def compileUseItemOnCreature(item: Card, creature: Card): String =
    s"USE ${item.instanceid} ${creature.instanceid}"

  def compilePassAction: String = "PASS"
}

class CardManager(cards: List[Card]) {
  val summonedEnemyCards:List[Card] = cards.filter(card => card.onEnemySideOfBoard)
  val cardsInMyHand:List[Card] = cards.filter(card => card.inMyHand)
  val MaxSummonedCards = 6

  // this will change when creatures with 'charge' are played
  val mySummonedCreatures:ListBuffer[Card] = cards.filter(card => card.onMySideOfBoard).to[ListBuffer]

  // TODO: right now using items or summoning is an either or proposition, with summoning taking priority
  // Both items and summoning can not assume they have full access to th player's mana
  def getActionsForTurn(totalMana: Int): String = {
    var currentMana = totalMana

    val cardsToSummon = getCardsToSummonThisTurn(totalMana, MaxSummonedCards - mySummonedCreatures.length)
    currentMana -= cardsToSummon.map(card => card.cost).sum
    val summoningActions = cardsToSummon.map(card =>
      CardManager.compileSummonAction(card)
    ).mkString(";")

    val attackActions = getAttackActionsForTurn
    val itemActions = getItemActionsForTurn(currentMana)

    Console.err.println(s"summoning actions: $summoningActions")
    Console.err.println(s"attack actions: $attackActions")
    Console.err.println(s"item actions: $itemActions")

    if (cardsToSummon.isEmpty && itemActions.nonEmpty && attackActions.isEmpty) {
      CardManager.compilePassAction
    } else {
      List(summoningActions, itemActions, attackActions).filter(_.nonEmpty).mkString(";")
    }
  }

  // TODO: support more than green cards
  private def getItemActionsForTurn(totalMana: Int): String = {
    if (mySummonedCreatures.isEmpty) {
      Console.err.println("Can't use green items because you don't have any summoned creatures")
      return ""
    }

    val greenItemCards = cardsInMyHand.filter(card => card.cardtype == CardType.GREEN.id)
    val mostExpensiveCardsICanAfford = getMostExpensiveCardsICanAfford(totalMana, greenItemCards)
    val creatureToBuff = mySummonedCreatures.sortBy(card => card.cost).last

    mostExpensiveCardsICanAfford.map(item =>
      CardManager.compileUseItemOnCreature(item, creatureToBuff)
    ).mkString(";")
  }

  private def getAttackActionsForTurn: String = {
    if (mySummonedCreatures.isEmpty) {
      Console.err.println("I have no creatures able to attack right now")
      return ""
    }

    val strongestFirstCreatureCounters = mySummonedCreatures.clone().sortBy(card => -card.attack)
    val counterMapping = mutable.Map.empty[Card, ListBuffer[Card]]

    getEnemyGuardCards.foreach { enemyCreature =>
      counterMapping.put(enemyCreature, ListBuffer.empty[Card])
      var currentDefense = enemyCreature.defense

      while (currentDefense > 0 && strongestFirstCreatureCounters.nonEmpty) {
        val myCounter = strongestFirstCreatureCounters.head
        counterMapping(enemyCreature) += myCounter
        currentDefense -= myCounter.attack
        strongestFirstCreatureCounters -= myCounter
      }

    }

    if (counterMapping.nonEmpty) {
      Console.err.println(s"MAPPING! $counterMapping")

      val tauntCounters = counterMapping.map {
        case (enemyCard, counterCards) =>
          counterCards.map(counterCard =>
            CardManager.compileCreatureToCreatureAttack(counterCard, enemyCard)
          ).mkString(";")
      }.mkString(";")

      val leftoverFace = strongestFirstCreatureCounters.map(card =>
        CardManager.compileCreatureToFaceAttack(card)
      ).mkString(";")
      Console.err.println(s"FACE! $leftoverFace")

      List(tauntCounters, leftoverFace).filter(_.nonEmpty).mkString(";")
    } else {
      mySummonedCreatures.map(card => CardManager.compileCreatureToFaceAttack(card)).mkString(";")
    }
  }

  private def getCardsToSummonThisTurn(currentMana: Int, spaceRemainingOnBoard: Int): List[Card] = {
    val creaturesInMyHand = cardsInMyHand.filter(card => card.cardtype == CardType.CREATURE.id)
    val sortedAvailableCreatures = getMostExpensiveCardsICanAfford(currentMana, creaturesInMyHand)

    var currentSpaceRemaining = spaceRemainingOnBoard
    val creaturesThatWillFit = sortedAvailableCreatures.takeWhile { _ =>
      val isSpaceAvailable = currentSpaceRemaining > 0
      currentSpaceRemaining -= 1
      isSpaceAvailable
    }

    mySummonedCreatures ++= creaturesThatWillFit.filter(_.abilities.hasCharge)
    creaturesThatWillFit
  }

  private def getEnemyGuardCards: List[Card] = summonedEnemyCards.filter(card => card.abilities.hasGuard)

  // TODO: Convert this to be based on mana efficiency. E.g. favor using as much mana as possible per turn
  private def getMostExpensiveCardsICanAfford(totalMana: Int, cardChoices: List[Card]): List[Card] = {
    val highestToLowestCostCards = cardChoices.sortBy(- _.cost)
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
