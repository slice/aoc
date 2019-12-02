package aoc.y2019

import aoc._
import aoc.util._

import cats.data.State

object Day2 extends Day {
  val code: List[Int] =
    slurp("inputs/day2.txt")
      .strip
      .split(",")
      .map(_.toInt)
      .to(List)

  type Memory = List[Int]
  case class Brain(memory: Memory, ip: Int, halted: Boolean = false)
  type Computer = State[Brain, Unit]

  def generateMemory(noun: Int, verb: Int): Memory =
    code.head :: noun :: verb :: code.drop(3)

  val part1Code = generateMemory(12, 2)

  val state: Computer = State {
    case original @ Brain(memory, ip, halted) =>
      // TODO: this is incredibly messy

      if (halted) {
        (original, ())
      } else {
        def binaryOp(f: (Int, Int) => Int): Memory = {
          val leftAddress :: rightAddress :: targetAddress :: _ =
            memory.drop(ip + 1 /* skip opcode */).take(3)

          memory.updated(
            targetAddress,
            f(memory(leftAddress), memory(rightAddress))
          )
        }

        if (memory(ip) == 99) {
          (Brain(memory, ip, halted = true), ())
        } else {
          val newMemory = memory(ip) match {
            case 1 => binaryOp(_ + _)
            case 2 => binaryOp(_ * _)
          }

          (Brain(newMemory, ip + 4), ())
        }
      }
  }

  def eval(initialMemory: Memory): Int = {
    val firstBrain = Brain(initialMemory, 0)

    def run(brain: Brain): Brain = state.runS(brain).value
    lazy val brains: LazyList[Brain] = run(firstBrain) #:: brains.map(run)

    brains.takeWhile(!_.halted).last.memory.head
  }

  def part1: Int = eval(part1Code)

  def part2: Int = {
    // you know what ... let's just brute force this

    val solutions = for {
      noun <- 0 to 99
      verb <- 0 to 99 if eval(generateMemory(noun, verb)) == 19690720
    } yield 100 * noun + verb

    solutions.head
  }
}
