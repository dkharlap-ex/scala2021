package scala2021.dkharlap.task06

import scala2021.dkharlap.task06.Sex.{FEMALE, MALE}

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
    people.foreach(person => println("Validate object " + person + ": " + printValidationResultToConsole(List(validateAndGetFirst(person)))))
    println("")
    println("Return validation results by validateAndGetAll()")
    people.foreach(person => println("Validate object " + person + ": " + printValidationResultToConsole(validateAndGetAll(person))))
    println("")
    println("Return validation results by validateAndGetAllInParallel()")
    people.foreach(person => println("Validate object " + person + ": " + printValidationResultToConsole(validateAndGetAllInParallel(person))))
  }

  def validateAndGetFirst(person: Person): Either[String, Person] = {
    for {
      _ <- validateName(person)
      _ <- validateAge(person)
      _ <- validateEmail(person)
      result <- validateSexAndHeight(person)
    } yield result
  }

  def validateAndGetAll(person: Person): List[Either[String, Person]] = {
    val nameValidation = validateName(person)
    val ageValidation = validateAge(person)
    val emailValidation = validateEmail(person)
    val complexValidation = validateSexAndHeight(person)
    List(nameValidation, ageValidation, emailValidation, complexValidation)
  }

  def validateAndGetAllInParallel(person: Person): List[Either[String, Person]] = {
    val f1 = Future(validateName(person))
    val f2 = Future(validateAge(person))
    val f3 = Future(validateEmail(person))
    val f4 = Future(validateSexAndHeight(person))

    val validation = for {
      f1Result <- f1
      f2Result <- f2
      f3Result <- f3
      f4Result <- f4
    } yield List(f1Result, f2Result, f3Result, f4Result)

    Await.result(validation, 2.second)
  }

  def validateName(person: Person): Either[String, Person] = {
    if (person.name.isEmpty) {
      Left("Name should not be empty")
    } else if (!person.name.matches("[A-Za-z]+")) {
      Left("Name should contain only latin letters")
    } else {
      Right(person)
    }
  }

  def validateAge(person: Person): Either[String, Person] = {
    if (person.age <= 0 || person.age >= 100) {
      Left("Age should be greater than 0 and less than 100")
    } else {
      Right(person)
    }
  }

  def validateEmail(person: Person): Either[String, Person] = {
    if (person.email.nonEmpty && !person.email.matches("^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$")) {
      Left("Email is not valid")
    } else {
      Right(person)
    }
  }

  def validateSexAndHeight(person: Person): Either[String, Person] = {
    if (person.sex == MALE) {
      if (person.height > 100) {
        Right(person)
      } else {
        Left("If sex is Male, then height should be greater than 100")
      }
    } else {
      Right(person)
    }
  }

  def printValidationResultToConsole(result: List[Either[String, Person]]): String = {
    if (result.nonEmpty) {
      val errors = result.filter(_.isLeft)
      if (errors.isEmpty) {
        "Validation is successful"
      } else {
        errors.map(_.left.getOrElse(" Unknown error")).toString()
      }
    } else {
      "Validation is successful"
    }
  }

}
