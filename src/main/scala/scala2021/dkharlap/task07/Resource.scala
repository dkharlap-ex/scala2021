package scala2021.dkharlap.task07

import java.io.File
import scala.language.reflectiveCalls

object Resource {

  def main(args: Array[String]): Unit = {
    withResource(8080)(param => {
      val connection = Connection(param.asInstanceOf[Int])
      println("Connection is created")
      connection
    }) (conn => conn.close()) { conn => conn.execute() }

    println("")
    withResource("scala2021/dkharlap/task07/test2.txt")(path => {
      val file = new File(path.toString)
      println("File is opened")
      file
    }) () { file => file.exists() }

    println("")
    withResource("scala2021/dkharlap/task07/test.txt")(path => {
      val inputStream = getClass.getClassLoader.getResourceAsStream(path.toString)
      println("FileInputStream is opened")
      inputStream
    }) (inputStream => {  inputStream.close()
                          println("FileInputStream is closed")
    })
    {
      inputStream =>
      val bytes = inputStream.available()
      println(s"Count bytes are $bytes")
    }
  }

  def withResource[A, B](resource: Any)
                        (creator: Any => A)
                        (closure: A => Unit = { _:A => println("No need to close resource")})
                        (executor: A => B): Any = {
    val instance = creator.apply(resource)
    val tryCleanResourceRes = try {
        Right(executor(instance))
      } catch {
        case e: Exception => Left(e)
      } finally {
        Right(closure.apply(instance))
      }

    tryCleanResourceRes match {
      case Right(_) => ()
      case Left(e) => throw e
    }
  }

  case class Connection(port: Int) {
    def close(): Unit = println("Connection is closed")
    def execute(): Unit = println("Execute query")
  }
}
