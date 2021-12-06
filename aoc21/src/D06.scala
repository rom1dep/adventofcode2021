import aocd.Problem

object D06 extends Problem(2021, 6):
  override def run(input: List[String]): Unit =
    val fishes: Array[Int] = input.head.split(",").map(_.toInt)

    part1((1 to 80).foldLeft(fishes)((prevFishes, _) => prevFishes.flatMap(t => if t == 0 then Array(6, 8) else Array(t - 1))).length)
    part2((1 to 256).foldLeft(fishes.groupMapReduce(identity)(_ => 1L)(_ + _))((prevFishes: Map[Int, Long], _) =>
      prevFishes.map((k, v) =>
        val newK = if k == 0 then 6 else k - 1
        val newV = if newK == 6 then prevFishes.getOrElse(0, 0L) + prevFishes.getOrElse(7, 0L) else v
        (newK, newV)
      ).updated(8, prevFishes.getOrElse(0, 0L))
    ).values.sum)
