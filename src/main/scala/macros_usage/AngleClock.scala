package macros_usage

package macros_usage

import macros.*

object AngleClock:
  @main
  def main(args: String*): Unit =
    val value = angleClockMacro(12, 30)
