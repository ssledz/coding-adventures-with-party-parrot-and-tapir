package solution

import java.lang.Math._
import scala.io.Source

object Ex1 extends App {

  private def load(fileName: String) = {
    val src   = Source.fromURI(classOf[Ex1.type].getClassLoader.getResource(fileName).toURI)
    val lines = src.getLines().toList

    val n = lines.head.toInt

    val m = lines.tail.zipWithIndex.flatMap { case (line, y) =>
      line.filterNot(_ == ' ').zipWithIndex.map { case (c, x) => ((x, y), c) }
    }
    src.close()

    val p = m.find { case (_, c) => c == '@' }

    (n, m, p.get._1)

  }

  private def solution(fileName: String): Char = {

    val (n, m, p @ (px, py)) = load(fileName)

    val peaks = m.filterNot { case (_, c) => c == '.' || c == '@' }

    val dist = peaks.map { case ((x, y), c) => (abs(x - px) + abs(y - py), c) }

    val stat = peaks.map(_._2).groupBy(identity).view.mapValues(_.size).toMap

    val sorted = dist.sortWith { case ((dist1, c1), (dist2, c2)) =>
      if (dist1 == dist2) stat(c1) > stat(c2)
      else dist1 < dist2
    }

    val ranking = sorted
      .map(_._2)
      .take(n)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toList
      .sortWith { case ((c1, n1), (c2, n2)) =>
        if (n1 == n2) stat(c1) > stat(c2)
        else n1 > n2
      }

    ranking.head._1
  }

  println("sample solution : " + solution("ex1-sample.txt"))
  println("solution        : " + solution("ex1.txt"))

}
