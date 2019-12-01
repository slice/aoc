package aoc

package object util {
  /** Read an entire file into an [[java.lang.String]]. */
  def slurp(filename: String): String = {
    val src = scala.io.Source.fromFile(filename)
    val text = src.mkString
    src.close
    text
  }
}
