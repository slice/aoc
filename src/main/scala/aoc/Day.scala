package aoc

trait Day {
  def main(args: Array[String]): Unit = {
    println(s"Part 1 = $part1")
    println(s"Part 2 = $part2")
  }

  def part1: Any
  def part2: Any
}
