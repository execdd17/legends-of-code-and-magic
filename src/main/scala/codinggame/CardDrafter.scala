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

  // TODO: These is a strange disconnect between this and CardManger Abilities
  object Ability extends Enumeration {
    type Ability = Value
    val BREAKTHROUGH, CHARGE, DRAIN, GUARD, LETHAL, WARD, NONE = Value
  }

  case class CardTypeDistribution(map: Map[Int, CardEvaluationCriteria])
  case class CardCostDistribution(map: Map[Int, CardEvaluationCriteria])
  case class CardAbilityDistribution(map: Map[Int, CardEvaluationCriteria])

  case class CardEvaluationCriteriaMap(map: Map[Int, CardEvaluationCriteria]) {
    def getPriorityAsInt(key: Int): Int = map(key).priority.id
  }

  case class CardEvaluationCriteria(priority: Priority, maxNum: Int)

  val ZooCardCostDistribution = CardCostDistribution(
    Map(
      CardCost.ZERO.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 5),
      CardCost.ONE.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 5),
      CardCost.TWO.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 10),
      CardCost.THREE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 10),
      CardCost.FOUR.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 5),
      CardCost.FIVE.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 0),
      CardCost.SIX.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      CardCost.SEVEN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      CardCost.EIGHT_AND_UP.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 1)
    )
  )

  val TitanCardCostDistribution = CardCostDistribution(
    Map(
      CardCost.ZERO.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 5),
      CardCost.ONE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 5),
      CardCost.TWO.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 10),
      CardCost.THREE.id -> CardEvaluationCriteria(priority = Priority.LOW, maxNum = 10),
      CardCost.FOUR.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 5),
      CardCost.FIVE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 0),
      CardCost.SIX.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 0),
      CardCost.SEVEN.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 0),
      CardCost.EIGHT_AND_UP.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 1)
    )
  )

  val MaxCreaturesDistribution = CardTypeDistribution(
    Map(
      CardType.CREATURE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 30),
      CardType.GREEN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      CardType.RED.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      CardType.BLUE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0)
    )
  )

  val HeavyCreaturesWithBuffsDistribution = CardTypeDistribution(
    Map(
      CardType.CREATURE.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 25),
      CardType.GREEN.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 5),
      CardType.RED.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      CardType.BLUE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
    )
  )

  val ModerateGuardFocusDistribution = CardAbilityDistribution(
    Map(
      Ability.BREAKTHROUGH.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.CHARGE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.DRAIN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.GUARD.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 10),
      Ability.LETHAL.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.WARD.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.NONE.id -> CardEvaluationCriteria(priority = Priority.LOW, maxNum = 0)
    )
  )

  val HeavyGuardAndBreakthroughDistribution = CardAbilityDistribution(
    Map(
      Ability.BREAKTHROUGH.id -> CardEvaluationCriteria(priority = Priority.HIGH, maxNum = 0),
      Ability.CHARGE.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.DRAIN.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.GUARD.id -> CardEvaluationCriteria(priority = Priority.HIGHEST, maxNum = 10),
      Ability.LETHAL.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.WARD.id -> CardEvaluationCriteria(priority = Priority.LOWEST, maxNum = 0),
      Ability.NONE.id -> CardEvaluationCriteria(priority = Priority.MEDIUM, maxNum = 0)
    )
  )
}

class CardDrafter(costDistribution: CardCostDistribution,
                  typeDistribution: CardTypeDistribution,
                  abilityDistribution: CardAbilityDistribution) {

  val deck: ListBuffer[Card] = ListBuffer.empty[Card]

  val manaBuckets: Int = CardCost.values.size
  val cardTypeBuckets: Int = CardType.values.size
  val abilityBuckets: Int = Ability.values.size

  val scoreMatrix: Array[Array[Array[Int]]] = Array.ofDim[Int](manaBuckets, cardTypeBuckets, abilityBuckets)

  for {
    i <- 0 until manaBuckets
    j <- 0 until cardTypeBuckets
    k <- 0 until abilityBuckets
  } scoreMatrix(i)(j)(k) = costDistribution.map(i).priority.id +
    typeDistribution.map(j).priority.id +
    abilityDistribution.map(k).priority.id

  //  for {
  //    i <- 0 until manaBuckets
  //    j <- 0 until cardTypeBuckets
  //    k <- 0 until abilityBuckets
  //  } Console.err.println(s"scoreMatrix($i)($j)($k) => ${scoreMatrix(i)(j)(k)}")

  def chooseCard(choices: Card*): Card = {
    val choice = chooseHelper(iteration = 1, choices.toList)
    Console.err.println(s"Choosing $choice")
    deck += choice
    choice
  }

  private def chooseHelper(iteration: Int, choices: List[Card]): Card = {
    val choice = choices.maxBy(card => scoreChoice(card))
    Console.err.println(s"Evaluating $choice")

    if (iteration == 3 || choiceIsValid(choice, costDistribution)) {
      choice
    } else {
      chooseHelper(iteration = iteration + 1, choices.filter(!_.equals(choice)))
    }
  }

  def choiceIsValid(choice: Card, costDistribution: CardCostDistribution): Boolean = {
    val manaCostToCount = deck.foldLeft(Map.empty[Int, Int]) { (map, card) =>
      if (map.get(card.cost).isEmpty) {
        map + (card.cost -> 1)
      } else {
        map + (card.cost -> (map(card.cost) + 1))
      }
    }

    val maxDesired = costDistribution.map(getManaCostIndexForCard(choice)).maxNum
    val actualAmount = manaCostToCount.getOrElse(choice.cost, 0)

    actualAmount < maxDesired
  }

  private def getManaCostIndexForCard(card: Card): Int =
    Math.min(card.cost, manaBuckets - 1) // -1 to offset 0 cost mana cards

  private def scoreChoice(card: Card): Int = {
    if (card.abilities.hasAbilities) {
      getBestScoreForAbilities(card)
    } else {
      val score = scoreMatrix(getManaCostIndexForCard(card))(card.cardtype)(Ability.NONE.id)
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

  private def addCardToDeck(card: Card): Unit = deck += card
}