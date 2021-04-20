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

    val advertisementNumberPerDomain: scala.collection.immutable.Map[String,Int] = countAdvertisementNumberPerDomain(stats)

    //Output data
    advertisementNumberPerDomain.keys.foreach { key =>
      println(advertisementNumberPerDomain(key) + " " + key)
    }
  }

  def countAdvertisementNumberPerDomain(stats: Array[String]): scala.collection.immutable.Map[String,Int] = {
    var advertisementNumberPerDomain: scala.collection.immutable.Map[String,Int] = scala.collection.immutable.Map()
    for (statItem <- stats) {
      val splittedStatItem: Array[String] = statItem.split(",")
      val advertisementNumber:Int = splittedStatItem(0).toInt
      val subDomains: Array[String] = splittedStatItem(1).split("\\.")
      var currentDomain: String = ""
      for (i <- subDomains.length - 1 to 0 by -1) {
        if (currentDomain.nonEmpty) {
            currentDomain = subDomains(i) + "." + currentDomain
        } else {
            currentDomain = subDomains(i)
        }

        if (advertisementNumberPerDomain.contains(currentDomain)) {
          val advertisementAmount:Int = advertisementNumberPerDomain(currentDomain) + advertisementNumber
          advertisementNumberPerDomain += currentDomain -> advertisementAmount
        } else {
          advertisementNumberPerDomain += currentDomain -> advertisementNumber
        }
      }
    }
    advertisementNumberPerDomain
  }

}
