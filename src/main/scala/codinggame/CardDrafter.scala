package codinggame

import codinggame.DraftingStratagem.Priority.Priority
import codinggame.DraftingStratagem._

import scala.collection.mutable.ListBuffer

object DraftingStratagem {
  object Priority extends Enumeration {
    type Priority = Value
    val LOWEST, LOW, MEDIUM, HIGH, HIGHEST = Value

    def priorityResolver(priorities: Priority*): Priority = {
      val lowestPriority = priorities.minBy(priority => priority.id)
      lowestPriority
    }
  }

  object CardType extends Enumeration {
    type CardType = Value
    val CREATURE, GREEN, RED, BLUE = Value
  }

  object CardCost extends Enumeration {
    type CardCost = Value
    val ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT_AND_UP = Value
  }

  object Abilities extends Enumeration {
    type Abilities = Value
    val BREAKTHROUGH, CHARGE, DRAIN, GUARD, LETHAL, WARD, NONE = Value
  }

  case class CardTypeDistribution(evaluationCriteriaMap: CardEvaluationCriteriaMap)
  case class CardCostDistribution(evaluationCriteriaMap: CardEvaluationCriteriaMap)
  case class CardAbilityDistribution(evaluationCriteriaMap: CardEvaluationCriteriaMap)

  case class CardEvaluationCriteriaMap(map: Map[Int, CardEvaluationCriteria]) {
    def getPriorityAsInt(key: Int):Int = map(key).priority.id
  }

  case class CardEvaluationCriteria(priority: Priority, maxNum: Int)

  val ZooCardCostDistribution = CardCostDistribution(
    CardEvaluationCriteriaMap(
      Map(
        CardCost.ZERO.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 5),
        CardCost.ONE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 5),
        CardCost.TWO.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 10),
        CardCost.THREE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 10),
        CardCost.FOUR.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 5),
        CardCost.FIVE.id -> CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0),
        CardCost.SIX.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        CardCost.SEVEN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        CardCost.EIGHT_AND_UP.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 1)
      )
    )
  )
//
//  val titanCardCostDistribution = CardCostDistribution(
//    zero = CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0),
//    one = CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0),
//    two = CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0),
//    three = CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0),
//    four = CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0),
//    five = CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 5),
//    six = CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 10),
//    seven = CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 10),
//    eightAndAbove = CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 10)
//  )

  val MaxCreaturesDistribution = CardTypeDistribution(
    CardEvaluationCriteriaMap(
      Map(
        CardType.CREATURE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 30),
        CardType.GREEN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        CardType.RED.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        CardType.BLUE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0)
      )
    )
  )

  val HeavyCreaturesWithFewGreenDistribution = CardTypeDistribution(
    CardEvaluationCriteriaMap(
      Map(
        CardType.CREATURE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 25),
        CardType.GREEN.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 5),
        CardType.RED.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        CardType.BLUE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      )
    )
  )

  val ModerateGuardFocusDistribution = CardAbilityDistribution(
    CardEvaluationCriteriaMap(
      Map(
        Abilities.BREAKTHROUGH.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        Abilities.CHARGE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        Abilities.DRAIN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        Abilities.GUARD.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 10),
        Abilities.LETHAL.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        Abilities.WARD.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
        Abilities.NONE.id -> CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0)
      )
    )
  )
}

class CardDrafter(costDistribution: CardCostDistribution,
                  typeDistribution: CardTypeDistribution,
                  abilityDistribution: CardAbilityDistribution) {

  val deck:ListBuffer[Card] = ListBuffer.empty[Card]

  val manaBuckets:Int = CardCost.values.size
  val cardTypeBuckets:Int = CardType.values.size
  val abilityBuckets:Int = Abilities.values.size

  val scoreMatrix:Array[Array[Array[Int]]] = Array.ofDim[Int](manaBuckets,cardTypeBuckets,abilityBuckets)

  for {
    i <- 0 until manaBuckets
    j <- 0 until cardTypeBuckets
    k <- 0 until abilityBuckets
  } scoreMatrix(i)(j)(k) = costDistribution.evaluationCriteriaMap.getPriorityAsInt(i) +
    typeDistribution.evaluationCriteriaMap.getPriorityAsInt(j) +
    abilityDistribution.evaluationCriteriaMap.getPriorityAsInt(k)

//  for {
//    i <- 0 until manaBuckets
//    j <- 0 until cardTypeBuckets
//    k <- 0 until abilityBuckets
//  } Console.err.println(s"scoreMatrix($i)($j)($k) => ${scoreMatrix(i)(j)(k)}")

  def chooseCard(choices: Card*): Card = {
    choices.maxBy(card => scoreChoice(card))
  }

  private def getManaCostIndexForCard(card: Card): Int =
    Math.min(card.cost, manaBuckets - 1)  // -1 to offset 0 cost mana cards

  private def scoreChoice(card: Card): Int = {
    if (card.abilities.hasAbilities) {
      getBestScoreForAbilities(card)
    } else {
      val score = scoreMatrix(getManaCostIndexForCard(card))(card.cardtype)(Abilities.NONE.id)
      Console.err.println(s"Score [$score] for card $card")
      score
    }
  }

  private def getBestScoreForAbilities(card: Card): Int = {
    var maxScore = 0

    card.abilities.abilities.foreach { char =>
      if (!char.equals('-')) {
        val abilityIndex = card.abilities.abilities.indexOf(char)
        val score = scoreMatrix(getManaCostIndexForCard(card))(card.cardtype)(abilityIndex)
        Console.err.println(s"Score [$score] for card $card")

        if (score > maxScore) {
          Console.err.println(s"setting new max score to $score")
          maxScore = score
        }
      }
    }

    maxScore
  }

  private def addCardToDeck(card: Card):Unit = deck += card
}