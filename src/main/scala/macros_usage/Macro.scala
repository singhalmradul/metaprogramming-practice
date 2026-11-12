package macros_usage

import macros.*

object Macro:
  // @main
  def main(args: String*): Unit =
    val value: Int = `macro`(12)
