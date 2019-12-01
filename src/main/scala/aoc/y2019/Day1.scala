package aoc.y2019

import aoc._
import aoc.util._

object Day1 extends Day {
  lazy val input = slurp("inputs/day1.txt")
    .linesIterator
    .map(_.toInt)
    .toList

  def fuel(mass: Int): Int = mass / 3 - 2

  def part1: Int = {
    input.map(fuel).sum
  }

  def part2: Int = {
    input.map(mass => {
      lazy val stream: LazyList[Int] = fuel(mass) #:: stream.map(fuel)
      stream.takeWhile(_ > 0).sum
    }).sum
  }
}
