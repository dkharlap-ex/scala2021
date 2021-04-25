package scala2021.dkharlap.task03

object DataEncoding {
  def main(args: Array[String]): Unit = {
    //Input data
    val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    val batches = encodeDirect(data)

    //Output data
    batches.foreach(println(_))
  }

  def encodeDirect(data: List[Symbol]): List[(Int, Symbol)] = {
    if (data.isEmpty) {
      Nil
    } else {
      val leftover = data.dropWhile(_ == data.head)
      val tuple = (data.size - leftover.length, data.head)
      List(tuple) ::: encodeDirect(leftover)
    }
  }

}
