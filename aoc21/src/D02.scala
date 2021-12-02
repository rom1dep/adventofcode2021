import aocd.Problem

object D02 extends Problem(2021, 2):
  case class Pos(hzl: Int = 0, dpt: Int = 0, aim: Int = 0):
    def product: Int = hzl * dpt

  override def run(input: List[String]): Unit =
    sol1(input)
    sol2(input)

  def sol1(input: List[String]): Int = part1 {
    input
      .foldLeft(Pos()) { (curPos: Pos, curInst: String) =>
        curInst.split(" ") match
          case Array("forward", amt) => curPos.copy(hzl = curPos.hzl + amt.toInt)
          case Array("up", amt)      => curPos.copy(dpt = curPos.dpt - amt.toInt)
          case Array("down", amt)    => curPos.copy(dpt = curPos.dpt + amt.toInt)
      }
      .product
  }

  def sol2(input: List[String]): Int = part2 {
    input
      .foldLeft(Pos()) { (curPos: Pos, curInst: String) =>
        curInst.split(" ") match
          case Array("forward", amt) =>
            curPos.copy(hzl = curPos.hzl + amt.toInt, dpt = curPos.dpt + curPos.aim * amt.toInt)
          case Array("up", amt)   => curPos.copy(aim = curPos.aim - amt.toInt)
          case Array("down", amt) => curPos.copy(aim = curPos.aim + amt.toInt)
      }
      .product
  }
