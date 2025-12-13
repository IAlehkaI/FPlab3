sealed trait DrinkType
case object Coffee extends DrinkType
case object Tea extends DrinkType

sealed trait Producer
case object Ahmad extends Producer
case object Lipton extends Producer
case object Nescafe extends Producer
case object Jacobs extends Producer

case class Drink(drinkType: DrinkType, producer: Producer, sugar: Int, milk: Boolean)
case class OrderHistory(drinks: List[Drink] = Nil)

object ProductCatalog {
  def getPrice(drinkType: DrinkType, producer: Producer): Float =
    (drinkType, producer) match {
      case (Tea, Ahmad) => 100.0f
      case (Tea, Lipton) => 125.0f
      case (Coffee, Nescafe) => 90.0f
      case (Coffee, Jacobs) => 110.0f
      case _ => 0.0f
    }

  def getValidProducers(drinkType: DrinkType): List[Producer] =
    drinkType match {
      case Tea => List(Ahmad, Lipton)
      case Coffee => List(Nescafe, Jacobs)
    }

  def price(drink: Drink): Float = {
    val basePrice = getPrice(drink.drinkType, drink.producer)
    val sugarCost = 5.0f * drink.sugar
    val milkCost = if (drink.milk) 20.0f else 0.0f
    basePrice + sugarCost + milkCost
  }

  def totalPrice(drinks: List[Drink]): Float =
    drinks.map(price).sum
}


