package scala2021.dkharlap.task01

object AdvertisementStatistics_v3 {
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
    val subDomains = stats.filter(statItem => statItem.nonEmpty && statItem.trim.split(",").length == 2).flatMap(statItem => {
      val Array(num, domain) = statItem.trim.split(",")
      val it = domain.split("\\.").tails
      it.collect({ case sd if sd.nonEmpty => (sd.mkString("."), num.toInt) })
    })
    subDomains.groupMapReduce(sd => sd._1)(sd => sd._2)((sd1, sd2) => sd1 + sd2)
  }
}
