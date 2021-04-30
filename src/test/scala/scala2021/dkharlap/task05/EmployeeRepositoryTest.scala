package scala2021.dkharlap.task05

import org.scalatest.funsuite.AnyFunSuite
import scala2021.dkharlap.task05.EmployeeRepository.{findEmployeeManagers, findManagerName, findManagerNameOrError}
import scala2021.dkharlap.task05.caseclasses.Info

class EmployeeRepositoryTest extends AnyFunSuite {

  private val expectedEmplInfoList = List(
    Info("Steve", "Marketing", "Steve"),
    Info("Mark", "Marketing", "Steve"),
    Info("Jane", "Marketing", "Steve"),
    Info("Samuel", "Sales", "Igor"),
    Info("Igor", "Sales", "Igor"),
    Info("Naveen", "IT", "Not Found"),
    Info("Christy", "Not Found", "Not Found"),
    Info("Megan", "Research", "Not Found"),
  )

  test("Check findManagerName() function") {
    assert(findManagerName("John") === None)
    assert(findManagerName("Steve") === Some("Steve"))
    assert(findManagerName("Mark") === Some("Steve"))
    assert(findManagerName("Igor") === Some("Igor"))
    assert(findManagerName("Christy") === None)
    assert(findManagerName("Naveen") === None)
    assert(findManagerName("Megan") === None)
  }

  test("Check findManagerNameOrError() function") {
    assert(findManagerNameOrError("John") === Left("There is no employee with name=John"))
    assert(findManagerNameOrError("Steve") === Right("Steve"))
    assert(findManagerNameOrError("Mark") === Right("Steve"))
    assert(findManagerNameOrError("Igor") === Right("Igor"))
    assert(findManagerNameOrError("Christy") === Left("Didn't find department by departmentId=5"))
    assert(findManagerNameOrError("Naveen") === Left("Didn't find employee by id=14"))
    assert(findManagerNameOrError("Megan") === Left("Didn't find manager by department=Research"))
  }

  test("Check findEmployeeManagers() function") {
    assert(expectedEmplInfoList == findEmployeeManagers())
  }

}
