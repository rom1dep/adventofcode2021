import aocd.Problem

object D07 extends Problem(2021, 7):
  override def run(input: List[String]): Unit =
    val initPos = input.head.split(",").map(_.toInt)
    part1((initPos.min to initPos.max).map(target => initPos.map(pos => (pos - target).abs).sum).min)
    part2((initPos.min to initPos.max).map(target => initPos.map(pos => (1 to (pos - target).abs).sum).sum).min)
