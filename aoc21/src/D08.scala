import aocd.Problem

object D08 extends Problem(2021, 8):

  override def run(input: List[String]): Unit =
    def parseInput(s: String): (Array[Set[Char]], List[Set[Char]]) = s.split(" \\| ") match
      case Array(l, r) => (l.split(" ").map(_.toSet).sortBy(_.size), r.split(" ").map(_.toSet).toList)

    part1(input.map(parseInput).map(_._2.map(w => if Array(2, 3, 4, 7).contains(w.size) then 1 else 0).sum).sum)
    part2 {
      def solve(signal: Array[Set[Char]], output:List[Set[Char]]): Int = signal match
        case Array(one, seven, four, fiveSeg1, fiveSeg2, fiveSeg3, sixSeg1, sixSeg2, sixSeg3, eight) =>
          //fiveSegs may be 2, 3, 5 ; sixSeg may be 0, 6, 9
          // among fiveSegs, only 5 has b (and d), and among sixSegs, only 0 doesn't
          val bd = four diff one
          val (fiveL, twoOrThree) = List(fiveSeg1, fiveSeg2, fiveSeg3).partition(bd.subsetOf)
          val (sixOrNine, zeroL) = List(sixSeg1, sixSeg2, sixSeg3).partition(bd.subsetOf)
          // 1 is a subset of 3 but not of 2 ; 1 is a subset of 9 but not of 6
          val (threeL, twoL) = twoOrThree.partition(one.subsetOf)
          val (nineL, sixL) = sixOrNine.partition(one.subsetOf)

          val cypher = List(zeroL.head, one, twoL.head, threeL.head, four, fiveL.head, sixL.head, seven, eight, nineL.head).zipWithIndex.toMap
          output.map(cypher).mkString.toInt

      input.map(parseInput).map(solve).sum
    }
