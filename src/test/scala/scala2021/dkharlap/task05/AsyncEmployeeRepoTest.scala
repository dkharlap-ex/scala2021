package scala2021.dkharlap.task05

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import scala2021.dkharlap.task05.EmployeeRepository.findManagerNameOrErrorAsync

class AsyncEmployeeRepoTest extends AsyncFlatSpec with TableDrivenPropertyChecks {

  private val testRezTable = Table(
      ("employee", "expected"),
      ("John", Left("There is no employee with name=John")),
      ("Steve", Right("Steve")),
      ("Mark", Right("Steve")),
      ("Igor", Right("Igor")),
      ("Christy", Left("Didn't find department by departmentId=5")),
      ("Naveen", Left("Didn't find employee by id=14")),
      ("Megan", Left("Didn't find manager by department=Research"))
    )

  "findManagerNameOrErrorAsync()" should "succeed" in {
    forAll(testRezTable) {
      (employee: String, expected: Either[String, String]) => findManagerNameOrErrorAsync(employee) map { result => assert(result === expected) }
    }
  }

}
