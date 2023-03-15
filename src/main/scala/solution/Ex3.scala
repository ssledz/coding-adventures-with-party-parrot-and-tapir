package solution

import scala.io.Source

object Ex3 extends App {

  def load(fileName: String): (Int, List[Char]) = {
    val lines = Source.fromResource(fileName).getLines().toList
    (lines.head.toInt, lines.tail.head.filterNot(_ == ' ').toList)
  }

  def solution(fileName: String): Int = {

    val (n, ps) = load(fileName)

//    println(n)
//    println(ps)

    def go(sub: List[(Int, List[Int], List[Char])], acc: List[List[Int]]): List[List[Int]] = sub match {
      case (_, tapirs, Nil) :: next => go(next, tapirs :: acc)
      case (i, tapirs, peaks) :: next =>
        val (xs, rest) = peaks.splitAt(n)
        val tps        = xs.zip(0 until n).filter { case (c, _) => c == '1' }
        val nsub = tps.map { case (_, ltp) =>
          val tp     = ltp + i
          val ni     = tp + n
          val npeaks = rest.drop(ltp)
          (ni, tp :: tapirs, npeaks)
        }
        go(nsub ::: next, acc)
      case Nil => acc
    }

    val sols = go(List((0, List.empty, ps)), List.empty)

//    println(sols)

    sols.map(_.size).min
  }

  println("sample solution : " + solution("ex3-sample.txt"))
  println("solution        : " + solution("ex3.txt"))

}
