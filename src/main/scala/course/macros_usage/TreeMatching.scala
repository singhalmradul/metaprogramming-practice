package course.macros_usage

import course.macros.*

object TreeMatching:
  def add(a: Int, b: Int): Int = a * b

  // @main
  def main(args: String*): Unit =
    treeMatching(add(4, 5))