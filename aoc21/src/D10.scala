import aocd.Problem

import scala.annotation.tailrec

object D10 extends Problem(2021, 10):
  val scoringP1 = Map(')' -> 3L, ']' -> 57L, '}' -> 1197L, '>' -> 25137L)
  val scoringP2 = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
  val closing   = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

  @tailrec def checker(rest: String, stack: List[Char] = List.empty): (Boolean, List[Char]) = // valid / stack
    if rest.isEmpty then (true, stack) // valid & may or may not be complete
    else if closing.contains(rest.head) && stack.head != closing(rest.head) then (false, List(rest.head)) // invalid
    else if closing.contains(rest.head) && stack.head == closing(rest.head) then checker(rest.tail, stack.tail)
    else checker(rest.tail, rest.head :: stack)

  override def run(input: List[String]): Unit =
    input.map(checker(_)).partition(_._1) match { case (valid, invalid) =>
      part1(invalid.map(c => scoringP1(c._2.head)).sum)
      part2 {
        val completions = valid.map(_._2.foldLeft(0L)((score, char) => score * 5 + scoringP2(char)))
        completions.sorted.drop(completions.length / 2).head
      }
    }
