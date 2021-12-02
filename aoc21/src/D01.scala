import aocd.Problem

object D01 extends Problem(2021, 1):
  override def run(input: List[String]): Unit =
    val data = input.map(_.toInt)
    sol1(data)
    sol2(data)

  def sol1(input: List[Int]): Int = part1(input.sliding(2).count(l => l.head < l.last))

  def sol2(input: List[Int]): Int = part2(input.sliding(3).map(_.sum).sliding(2).count(l => l.head < l.last))
