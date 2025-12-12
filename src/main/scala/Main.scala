import scala.io.StdIn

object Main {
  def consoleInput: LazyList[String] = {
    def loop: LazyList[String] = {
      print("> ")
      val line = StdIn.readLine()
      if (line == null || line.trim.isEmpty) LazyList.empty
      else line.trim #:: loop
    }
    loop
  }

  def process(history: OrderHistory, stream: LazyList[String], count: Int): Unit = {
    if (stream.isEmpty) {
      showFinalOrder(history.drinks, count - 1)
      return
    }

    Parser.readDrink(stream) match {
      case Right((drink, rest)) =>
        val newHistory = OrderHistory(drink :: history.drinks)
        val finalPrice = ProductCatalog.price(drink)

        println(s"\n=== Напиток #$count ===")
        println(s"Тип: ${drink.drinkType}")
        println(s"Производитель: ${drink.producer}")
        println(s"Сахар: ${drink.sugar} шт (5р/шт)")
        println(s"Молоко: ${if (drink.milk) "да (20р)" else "нет"}")
        println(s"Цена: $finalPrice руб")

        print("Продолжить заказ? (y/n): ")
        val continue = StdIn.readLine().toLowerCase()
        if (continue == "y" || continue == "да") {
          process(newHistory, rest, count + 1)
        } else {
          showFinalOrder(newHistory.drinks, count)
        }

      case Left(error) =>
        println(s"Ошибка: ${error.msg}")
        process(history, stream.drop(4), count)
    }
  }

  def showFinalOrder(drinks: List[Drink], count: Int): Unit = {
    println("\n" + "=" * 60)
    println(s"          ИТОГОВЫЙ ЗАКАЗ ($count напитков)")
    println("=" * 60)

    drinks.zipWithIndex.reverse.foreach { case (drink, i) =>
      val price = ProductCatalog.price(drink)
      println(s"${i+1}. ${drink.drinkType} ${drink.producer} (сахар:${drink.sugar}, молоко:${if(drink.milk)"да" else "нет"}) = $price руб")
    }

    val total = ProductCatalog.totalPrice(drinks)
    println("=" * 60)
    println(s"Общая сумма: $total руб")
  }

  def main(args: Array[String]): Unit = {
    println("=== МЕНЮ НАПИТКОВ ===")
    println("Чаи: Ahmad(100р), Lipton(125р) | Кофе: Nescafe(90р), Jacobs(110р)")
    println("Сахар: 5р/шт (0-10), Молоко: 20р")
    println("Вводите по 4 строки: тип производитель сахар молоко(y/n)")
    println("=" * 50)

    process(OrderHistory(), consoleInput, 1)
  }
}
