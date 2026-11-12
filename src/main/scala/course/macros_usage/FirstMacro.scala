package course.macros_usage

import course.macros.*


object FirstMacro:
  // @main
  def main(args: String*): Unit =
    firstMacro(5, "Scala is amazing")
    val int = 23
    val string = "MetaProgramming"
    firstMacroInlineArguments(int, string)
    firstMacroInlineArguments(2 * 4 % 2, "scala".repeat(3))