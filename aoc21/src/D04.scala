import aocd.Problem

import scala.annotation.tailrec

object D04 extends Problem(2021, 4):
  class Board(values: Array[Array[(Int, Boolean)]]):
    def play(draw: Int) = Board(values.flatten.map((cell, seen) => (cell, seen || cell == draw)).grouped(5).toArray)
    def won = values.map(_.count(_._2)).contains(5) || values.transpose.map(_.count(_._2)).contains(5)
    def sumUnmarked = values.flatten.filter(_._2 == false).map(_._1).sum

  override def run(input: List[String]): Unit =
    val draws: List[Int] = input.head.split(",").map(_.toInt).toList
    val boards: Array[Board] = input.drop(2)
      .mkString("\n").split("\n\n") // ugly, but splits into a new group for every empty line
      .map(_.split("\n").map(_.split(" ").filterNot(_ == "") // trims whitespace
        .map(s => (s.toInt, false)))).map(Board(_))

    @tailrec def bingo(winCondition: Array[Board] => Boolean, scoringMethod: (Array[Board], Array[Board], Int) => Int)
                      (remDraws: List[Int] = draws, prevBoards: Array[Board] = boards): Int =
      val newBoards = prevBoards.map(_.play(remDraws.head))
      if winCondition(newBoards) then scoringMethod(prevBoards, newBoards, remDraws.head)
      else bingo(winCondition, scoringMethod)(remDraws.tail, newBoards)

    part1(bingo(
      winCondition = (newBoards: Array[Board]) => newBoards.exists(_.won),
      scoringMethod = (_, newBoards: Array[Board], drawn: Int) => drawn * newBoards.dropWhile(!_.won).head.sumUnmarked
    )())

    part2(bingo(
      winCondition = (newBoards: Array[Board]) => newBoards.count(!_.won) == 0,
      scoringMethod = (prevBoards: Array[Board], _, drawn: Int) => drawn * (prevBoards.dropWhile(_.won).head.sumUnmarked - drawn)
    )())
