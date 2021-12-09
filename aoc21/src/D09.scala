import aocd.Problem

import scala.annotation.tailrec

object D09 extends Problem(2021, 9):
  case class Point(x: Int, y: Int, height: Int)
  case class Cave(points: Array[Array[Point]]):
    val (width, height)         = (points(0).length, points.length)
    val lowPoints: Array[Point] = points.flatten.filterNot(p => neighbors(p).exists(n => n.height <= p.height))

    def neighbors(p: Point, basin: Boolean = false): Set[Point] = (for
      x <- 0.max(p.x - 1) to (width - 1).min(p.x + 1)
      y <- 0.max(p.y - 1) to (height - 1).min(p.y + 1)
      if (x, y) != (p.x, p.y) && (x == p.x || y == p.y) // skip self and diagonals
      if !basin || points(y)(x).height != 9             // skip edges of the basin
    yield points(y)(x)).toSet

    def basinSize(lowPoint: Point): Int =
      @tailrec def findBasin(
          p: Point = lowPoint,
          seen: Set[Point] = Set(),
          next: Set[Point] = neighbors(lowPoint, true)
      ): Set[Point] =
        val newNext = neighbors(p, basin = true) -- seen ++ next
        if newNext.isEmpty then seen + p
        else findBasin(newNext.head, seen + p, newNext.tail)

      findBasin().size

  override def run(input: List[String]): Unit =
    val cave = Cave((for (ys, y) <- input.zipWithIndex.toArray; (xv, x) <- ys.zipWithIndex; height = xv.toString.toInt
    yield Point(x, y, height)).grouped(input.head.length).toArray)
    part1(cave.lowPoints.map(_.height + 1).sum)
    part2(cave.lowPoints.map(cave.basinSize).sorted.reverse.take(3).product)
