package scala2021.dkharlap.task01

object AdvertisementStatistics {
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
    for (statItem <- stats) {
      val splittedStatItem = statItem.split(",")
      val advertisementNumber = splittedStatItem(0).toInt
      val subDomains = splittedStatItem(1).split("\\.")
      var currentDomain = ""
      for (i <- subDomains.length - 1 to 0 by -1) {
        if (currentDomain.nonEmpty) {
            currentDomain = subDomains(i) + "." + currentDomain
        } else {
            currentDomain = subDomains(i)
        }

        if (advertisementNumberPerDomain.contains(currentDomain)) {
          val advertisementAmount = advertisementNumberPerDomain(currentDomain) + advertisementNumber
          advertisementNumberPerDomain += currentDomain -> advertisementAmount
        } else {
          advertisementNumberPerDomain += currentDomain -> advertisementNumber
        }
      }
    }
    advertisementNumberPerDomain
  }
}
