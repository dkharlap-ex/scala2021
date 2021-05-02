package scala2021.dkharlap.task06

import scala2021.dkharlap.task06.Sex.Sex

case class Person(name:String, age: Int, email: String, sex: Sex, height: Double)

object Sex extends Enumeration {
  type Sex = Value
  val MALE, FEMALE = Value
}
