import aocd.Problem

object D05 extends Problem(2021, 5):
  case class Point(x: Int, y: Int)
  case class Line(s: Point, e: Point):
    def straight: Boolean = s.x == e.x || s.y == e.y
    def allPoints: List[Point] =
      val (dx, dy) = (e.x compare s.x, e.y compare s.y)
      val steps = ((s.x - e.x).abs).max((s.y - e.y).abs)
      (for i <- 0 to steps yield Point(s.x + i * dx, s.y + i * dy)).toList

  override def run(input: List[String]): Unit =
    val lines = input.map(_.split(",| -> ") match {
      case Array(x1: String, y1: String, x2: String, y2: String) =>
        Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    })

    part1(lines.filter(_.straight).flatMap(_.allPoints).groupMapReduce(identity)(_ => 1)(_ + _).count(_._2 > 1))
    part2(lines.flatMap(_.allPoints).groupMapReduce(identity)(_ => 1)(_ + _).count(_._2 > 1))
