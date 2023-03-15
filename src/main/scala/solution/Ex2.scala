package solution

import scala.io.Source

object Ex2 extends App {

  private def solution(fileName: String): Long = {
    val line = Source.fromResource(fileName).getLines().toList.head

    val dec = line.split(' ').toList.map { bits =>
      val ones = bits.count(_ == '1')
      if (ones >= 2) "1" else "0"
    }

    val ints = dec
      .grouped(8)
      .toList
      .map(bits => Integer.parseInt(bits.mkString, 2))

    ints.sum
  }

  println("sample solution : " + solution("ex2-sample.txt"))
  println("solution        : " + solution("ex2.txt"))
}
