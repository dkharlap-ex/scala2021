package scala2021.dkharlap.task01

object AdvertisementStatistics_v2 {
  def main(args: Array[String]): Unit = {
    //Input data
    val stats = Array(
      "900,google.com",
      "60,mail.yahoo.com",
      "10,mobile.sports.yahoo.com",
      "40,sports.yahoo.com",
      "10,stackoverflow.com",
      "15,en.wikipedia.org",
      "3,es.wikipedia.org",
      "25,scala-lang.org"
    )

    val advertisementNumberPerDomain = countAdvertisementNumberPerDomain(stats)

    //Output data
    advertisementNumberPerDomain.keys.foreach { key =>
      println(advertisementNumberPerDomain(key) + " " + key)
    }
  }

  def countAdvertisementNumberPerDomain(stats: Array[String]): Map[String,Int] = {
    var advertisementNumberPerDomain: Map[String,Int] = Map()
    stats.filter(statItem => statItem.nonEmpty).map(statItem => {
      val splittedStatItem = statItem.split(",")
      val advertisementNumber = splittedStatItem(0).toInt

      var currentDomain = ""
      if (splittedStatItem.length > 1) {
        splittedStatItem(1).split("\\.").reverse.map(subDomain => {
          if (currentDomain.nonEmpty) {
            currentDomain = subDomain + "." + currentDomain
          } else {
            currentDomain = subDomain
          }

          if (advertisementNumberPerDomain.contains(currentDomain)) {
            val advertisementAmount = advertisementNumberPerDomain(currentDomain) + advertisementNumber
            advertisementNumberPerDomain += currentDomain -> advertisementAmount
          } else {
            advertisementNumberPerDomain += currentDomain -> advertisementNumber
          }
        })
      }
    })
    advertisementNumberPerDomain
  }
}
