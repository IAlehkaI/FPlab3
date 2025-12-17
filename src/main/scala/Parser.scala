import scala.util.Try

object Parser {
  def readDrink(ll: LazyList[String]): Either[ParseError, (Drink, LazyList[String])] = {
    ll match {
      case typeStr #:: prodStr #:: sugarStr #:: milkStr #:: tail =>
        val result = for {
          drinkType <- parseDrinkType(typeStr)
          producer <- parseProducerForType(prodStr, drinkType)
          sugar <- parseSugar(sugarStr)
          //sugar <- Try(sugarStr.toInt).toEither.left.map(_ => ParseError("Сахар - число"))
          //_ <- validateSugar(sugar)
          milk <- parseMilk(milkStr)
        } yield Drink(drinkType, producer, sugar, milk)

        result.map((_, tail))

      case _ =>
        Left(ParseError("Нужно 4 строки: тип производитель сахар молоко"))
    }
  }

  private def parseDrinkType(str: String): Either[ParseError, DrinkType] =
    str.toLowerCase match {
      case "чай" | "tea" => Right(Tea)
      case "кофе" | "coffee" => Right(Coffee)
      case _ => Left(ParseError("Тип: 'чай' или 'кофе'"))
    }

  private def parseSugar(str: String): Either[ParseError, Int] =
    Try(str.toInt).toEither
      .left.map(_ => ParseError("Сахар - число"))
      .flatMap(validateSugar)

  private def validateSugar(sugar: Int): Either[ParseError, Int] =
    if (sugar >= 0 && sugar <= 10) Right(sugar)
    else Left(ParseError("Сахар: 0-10"))

  private def parseProducerForType(str: String, drinkType: DrinkType): Either[ParseError, Producer] = {
    val producer = str.toLowerCase match {
      case "ahmad" => Right(Ahmad)
      case "lipton" => Right(Lipton)
      case "nescafe" => Right(Nescafe)
      case "jacobs" => Right(Jacobs)
      case _ => Left(ParseError("Производитель: Ahmad, Lipton, Nescafe, Jacobs"))
    }

    producer.flatMap { prod =>
      val valid = ProductCatalog.getValidProducers(drinkType)
      if (valid.contains(prod)) Right(prod)
      else Left(ParseError(s"$prod не производит ${drinkType.toString.toLowerCase}"))
    }
  }

  private def parseMilk(str: String): Either[ParseError, Boolean] =
    str.toLowerCase match {
      case "y" | "да" | "milk" | "молоко" => Right(true)
      case "n" | "нет" | "no" => Right(false)
      case _ => Left(ParseError("Молоко: y/n или да/нет"))
    }
}

case class ParseError(msg: String)
