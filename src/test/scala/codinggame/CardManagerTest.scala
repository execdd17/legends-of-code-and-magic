package codinggame

import org.scalatest._

class CardManagerTest
  extends FlatSpec
  with Matchers
  with BeforeAndAfterEach {

  var cardInMyHand1:Card = _
  var cardInMyHand2:Card = _
  var cardInMyHand3:Card = _

  var cardOnMySide1:Card = _

  var cardOnTheirSideWithGuard:Card = _

  override protected def beforeEach(): Unit = {
    cardInMyHand1 = Card(
      cardnumber = 0,
      instanceid = 1,
      location = Card.IN_MY_HAND,
      cardtype = 0,
      cost = 3,
      attack = 4,
      defense = 4,
      abilities = CardAbilities("")
    )

    cardInMyHand2 = cardInMyHand1.copy(instanceid = 2, cost = 4)
    cardInMyHand3 = cardInMyHand1.copy(instanceid = 3, cost = 5)

    cardOnMySide1 = cardInMyHand1.copy(instanceid = 4, location = Card.ON_MY_SIDE)

    cardOnTheirSideWithGuard = cardInMyHand1.copy(
      instanceid = 5,
      location = Card.ON_THEIR_SIDE,
      abilities = CardAbilities("G")
    )
  }

  "A CardManager" should "properly initialize" in {
    val cardManager = new CardManager(List(cardInMyHand1))
    cardManager.cardsInMyHand.size shouldBe 1
    cardManager.mySummonedCards.size shouldBe 0
    cardManager.summonedEnemyCards.size shouldBe 0
  }

  it should "select the highest card to summon" in {
    val cardManager = new CardManager(List(cardInMyHand1,cardInMyHand2,cardInMyHand3))
    val actions = cardManager.getActionsForTurn(currentMana = 4)
    actions shouldEqual s"SUMMON ${cardInMyHand2.instanceid}"
  }

  it should "select the highest cards to summon, when able" in {
    val cardManager = new CardManager(List(cardInMyHand1,cardInMyHand2))
    val actions = cardManager.getActionsForTurn(currentMana = 7)
    actions shouldEqual s"SUMMON ${cardInMyHand2.instanceid};SUMMON ${cardInMyHand1.instanceid}"
  }

  it should "prioritize attacking enemy guard summons" in {
    val cardManager = new CardManager(List(cardOnMySide1, cardOnTheirSideWithGuard))
    val actions = cardManager.getActionsForTurn(currentMana = 3)
    actions shouldEqual s"ATTACK ${cardOnMySide1.instanceid} ${cardOnTheirSideWithGuard.instanceid}"
  }

  it should "attack the face when no enemy guard summons are in play" in {
    val cardManager = new CardManager(List(cardOnMySide1))
    val actions = cardManager.getActionsForTurn(currentMana = 3)
    actions shouldEqual s"ATTACK ${cardOnMySide1.instanceid} -1"
  }

  it should "utilize a summon immediately that has the Charge ability" in {
    pending
  }
}