package scala2021.dkharlap.task04

object Coins_v2 {
  def main(args: Array[String]): Unit = {
    val coins = List[Int](2,6,4)
    println("Change is possible=" + checkChangePossibility(5, coins))
  }

  def checkChangePossibility(money: Int, coins: List[Int]): Boolean = {
    def canChange(money: Int, coins: List[Int], coinCheckIndex: Int): Boolean = {
      if (money == 0) true
      else if (money < 0) false
      else if (coins.length == coinCheckIndex) false
      else {
        canChange(money - coins.apply(coinCheckIndex), coins, coinCheckIndex) || canChange(money, coins, coinCheckIndex + 1)
      }
    }
    canChange(money, coins, 0)
  }

}
