package solution

import scala.io.Source

object Ex4 extends App {

  def load(fileName: String) = {
    val lines = Source.fromResource(fileName).getLines().toList

    val List(m, n) = lines.head.split(' ').toList

    (m.toInt, n.toInt, lines.tail.head)
  }

  private def solution(fileName: String): String = {
    val (m, n, line) = load(fileName)

    val digs = line.map(_.toString.toLong).toList

    def seq(n: Int): List[List[Long]] =
      digs.drop(n).foldLeft(List(digs.take(n))) { case (acc @ (h :: _), d) =>
        val ne = h.drop(1) ::: List(d)
        ne :: acc
      }

    val s1 = seq(m).reverse.map(xs => xs.map(_.toString).mkString -> xs.map(_.toDouble).product).maxBy(_._2)
    val s2 = seq(n).reverse.map(xs => xs.map(_.toString).mkString -> xs.sum).minBy(_._2)
    val s3 = s1._1.zip(s2._1).count { case (a, b) => a != b }

//    println(m + " " + n)
//    println(line)
//    println(digs)
//    println(s1)
//    println(s2)
//    println(s3)

    s1._1 + s2._1 + s3.toString
  }

  println("sample solution : " + solution("ex4-sample.txt"))
  println("solution        : " + solution("ex4.txt"))
}
