package scala2021.dkharlap.task05

import scala2021.dkharlap.task05.caseclasses.{Department, Employee, Info, Manager}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object EmployeeRepository {

  val employees = List(
    Employee(1, "Steve", 1),
    Employee(3, "Mark", 1),
    Employee(4, "Jane", 1),
    Employee(7, "Samuel", 2),
    Employee(10, "Igor", 2),
    Employee(11, "Naveen", 4),
    Employee(12, "Christy", 5),
    Employee(15, "Megan", 3)
  )

  val departments = List(
    Department(1, "Marketing"),
    Department(2, "Sales"),
    Department(3, "Research"),
    Department(4, "IT")
  )

  val managers = List(
    Manager("Marketing", 1),
    Manager("Sales", 10),
    Manager("IT", 14)
  )

  def findManagerName(employeeName: String): Option[String] = {
    for {
      employee <- employees.find(e => e.name == employeeName)
      department <- departments.find(d => d.id == employee.departmentId)
      manager <- managers.find(m => m.department == department.name)
      m_employee <- employees.find(e => e.id == manager.employeeId)
    } yield m_employee.name
  }

  def findManagerNameOrError(employeeName: String): Either[String, String] = {
    employees.find(e => e.name == employeeName) match {
      case Some(e) =>
        departments.find(d => d.id == e.departmentId) match {
          case Some(d) =>
            managers.find(m => m.department == d.name) match {
              case Some(m) =>
                employees.find(m_empl => m_empl.id == m.employeeId) match {
                  case Some(m_empl) => Right(m_empl.name)
                  case None => Left("Didn't find employee by id=" + m.employeeId)
                }
              case None => Left("Didn't find manager by department=" + d.name)
            }
          case None => Left("Didn't find department by departmentId=" + e.departmentId)
        }
      case None => Left("There is no employee with name=" + employeeName)
    }
  }

  def findManagerNameOrErrorAsync(employeeName: String): Future[Either[String, String]] = {
    Future {
      findManagerNameOrError(employeeName)
    }
  }

  def findEmployeeManagers(): List[Info] = {
    val defaultValue = "Not Found"
    employees.map(employee => {
      departments.find(d => d.id == employee.departmentId) match {
        case Some(d) =>
          managers.find(m => m.department == d.name) match {
            case Some(m) =>
              employees.find(m_empl => m_empl.id == m.employeeId) match {
                case Some(m_empl) => Info(employee.name, d.name, m_empl.name)
                case None => Info(employee.name, d.name, defaultValue)
              }
            case None => Info(employee.name, d.name, defaultValue)
          }
        case None => Info(employee.name, defaultValue, defaultValue)
      }
    })
  }
}
