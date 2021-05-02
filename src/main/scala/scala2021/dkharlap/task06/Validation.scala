package scala2021.dkharlap.task06

import scala2021.dkharlap.task06.Sex.{FEMALE, MALE, Sex}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Validation {

  def main(args: Array[String]): Unit = {
    val people = List(
      Person("Denis", 12, "den@yandex.ru", MALE, 140),
      Person("", 13, "den4@yandex.ru", MALE, 142),
      Person("Денис", 15, "den2@yandex.ru", MALE, 152.1),
      Person("Den11", 16, "den3@yandex.ru", MALE, 154.6),
      Person("Helen", 18, "hel@yandex.ru", FEMALE, 170.4),
      Person("Amanda", -3, "am@yandex.ru", FEMALE, 80),
      Person("Kate", 15, "am@yandex.rururu", FEMALE, 185.9),
      Person("Amanda_2", 24, "am&you@yandex.ru", FEMALE, 190),
      Person("Andrey_123", 100, "andyandex.ru", MALE, 99.8),
      Person("Chad", 34, "chad@yandex.ru", MALE, 56.1),
    )
    println("Return validation results by validateAndGetFirst()")
    people.foreach(person => println(validateAndGetFirst(person)))
    println("")
    println("Return validation results by validateAndGetAll()")
    people.foreach(person => println(validateAndGetAll(person)))
    println("")
    println("Return validation results by validateAndGetAllInParallel()")
    people.foreach(person => println(validateAndGetAllInParallel(person)))
  }

  def validateAndGetFirst(person: Person): String = {
    val validation = validateName(person.name) match {
      case nameValidation if nameValidation.isLeft => nameValidation.left.getOrElse("Name Validation - Unknown error")
      case nameValidation if nameValidation.isRight =>
        validateAge(person.age) match {
          case ageValidation if ageValidation.isLeft => ageValidation.left.getOrElse("Age Validation - Unknown error")
          case ageValidation if ageValidation.isRight =>
            validateEmail(person.email) match {
              case emailValidation if emailValidation.isLeft => emailValidation.left.getOrElse("Email Validation - Unknown error")
              case emailValidation if emailValidation.isRight =>
                validateSexAndHeight(person.sex, person.height) match {
                  case complexValidation if complexValidation.isLeft => complexValidation.left.getOrElse("Complex Validation (Sex and Height) - Unknown error")
                  case complexValidation if complexValidation.isRight => "Validation is successful"
                }
            }
        }
    }
    "Validate object " + person + ": " + validation
  }

  def validateAndGetAll(person: Person): String = {
    val nameValidation = validateName(person.name)
    val ageValidation = validateAge(person.age)
    val emailValidation = validateEmail(person.email)
    val complexValidation = validateSexAndHeight(person.sex, person.height)
    val errors = List(nameValidation, ageValidation, emailValidation, complexValidation).filter(_.isLeft)
    "Validate object " + person + ": ".concat(if (errors.isEmpty) "Validation is successful" else errors.map(_.left.getOrElse(" Unknown error")).toString())
  }

  def validateAndGetAllInParallel(person: Person): String = {
    val f1 = Future(validateName(person.name))
    val f2 = Future(validateAge(person.age))
    val f3 = Future(validateEmail(person.email))
    val f4 = Future(validateSexAndHeight(person.sex, person.height))

    val validation = for {
      f1Result <- f1
      f2Result <- f2
      f3Result <- f3
      f4Result <- f4
    } yield "Validate object " + person + ": ".concat(if (!List(f1Result, f2Result, f3Result, f4Result).exists(_.isLeft)) "Validation is successful"
          else List(f1Result, f2Result, f3Result, f4Result).filter(_.isLeft).map(_.left.getOrElse(" Unknown error")).toString())

    Await.result(validation, 2.second)
  }

  def validateName(name: String): Either[String, String] = {
    name match {
      case value if value.isEmpty => Left("Name should not be empty")
      case value if !value.matches("[A-Za-z]+") => Left("Name should contain only latin letters")
      case _ => Right(name)
    }
  }

  def validateAge(age: Int): Either[String, Int] = {
    age match {
      case value if (value <= 0 || value >= 100) => Left("Age should be greater than 0 and less than 100")
      case _ => Right(age)
    }
  }

  def validateEmail(email: String): Either[String, String] = {
    email match {
      case value if value.isEmpty => Right(email)
      case value if !value.matches("^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$") => Left("Email is not valid")
      case _ => Right(email)
    }
  }

  def validateSexAndHeight(sex: Sex, height: Double): Either[String, (Sex, Double)] = {
    if (sex == MALE) {
      if (height > 100) {
        Right((sex, height))
      } else {
        Left("If sex is Male, then height should be greater than 100")
      }
    } else {
      Right((sex, height))
    }
  }

}
