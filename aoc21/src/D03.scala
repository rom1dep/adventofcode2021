import aocd.Problem
import java.lang.Integer.parseInt
import scala.annotation.tailrec

object D03 extends Problem(2021, 3):

  override def run(input: List[String]): Unit =
    def occurrences(a: List[Char]) = a.groupMapReduce(identity)(_ => 1)(_ + _)

    part1 {
      val occMap  = input.transpose.map(occurrences)
      val gamma   = parseInt(occMap.map(_.maxBy(_._2)).map(_._1).mkString, 2)
      val epsilon = parseInt(occMap.map(_.minBy(_._2)).map(_._1).mkString, 2)
      gamma * epsilon
    }

    part2 {
      @tailrec def filter(method: Map[Char, Int] => Char)(candidates: List[String] = input, ix: Int = 0): Int =
        candidates match
          case last :: Nil => parseInt(last, 2)
          case _ =>
            val keep: Char = method(candidates.transpose.map(occurrences)(ix))
            filter(method)(candidates.filter(_(ix) == keep), ix + 1)

      val oxygen = filter(method = (occ: Map[Char, Int]) => if occ('1') >= occ('0') then '1' else '0')()
      val co2    = filter(method = (occ: Map[Char, Int]) => if occ('1') < occ('0') then '1' else '0')()
      oxygen * co2
    }
