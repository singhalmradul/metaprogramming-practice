package course.macros_usage

import course.macros.*

object DefDefs:
  // @main
  def main(args: String*): Unit =
    val x = buildDefDef(5, "scala", false)