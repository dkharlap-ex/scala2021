package scala2021.dkharlap.task09

import scala2021.dkharlap.task09.MoneyCalculator.Currency.{Currency, EUR, GBP, USD}

object MoneyCalculator {

  def main(args: Array[String]): Unit = {
    val result = 42(USD) + 35(EUR)
    val resultToPound = result to GBP
    println("42(USD) + 35(EUR) = " + resultToPound + "(GBP)")

    val result2 = 35.62(GBP) + 14.1236(USD)
    val resultToEuro = result2 to EUR
    println("35.62(GBP) + 14.1236(USD) = " + resultToEuro + "(EUR)")
  }

  implicit class MoneyConverter(arg: Double) {
    def apply(currency: Currency): Money = {
      Money(BigDecimal(arg).setScale(3, BigDecimal.RoundingMode.HALF_UP), currency)
    }
  }

  case class Money(value : BigDecimal, currency: Currency) {
    val rates = Map(USD -> 1.00, EUR -> 1.22, GBP -> 1.42)

    def + (tValue: Money): Money = {
      val sum = to(USD) + tValue.to(USD)
      Money(sum, USD)
    }

    def to (targetCurrency: Currency): BigDecimal = {
      if (currency != targetCurrency) {
        val valueToUSD = value * rates(currency)
        val valueToTargetCurrency = valueToUSD / rates(targetCurrency)
        valueToTargetCurrency.setScale(3, BigDecimal.RoundingMode.HALF_UP)
      } else {
        value
      }
    }
  }

  object Currency extends Enumeration {
    type Currency = Value
    val USD,EUR,GBP = Value
  }

}
