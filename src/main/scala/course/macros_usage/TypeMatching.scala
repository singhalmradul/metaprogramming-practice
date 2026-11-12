package course.macros_usage

import course.macros.*

object TypeMatching:
  // @main
  def main(args: String*): Unit =
    typeMatching[Int]
    typeMatching[Int => String]
    typeMatching[(Int, String, Int)]