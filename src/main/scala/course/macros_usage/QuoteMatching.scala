package course.macros_usage

import course.macros.*

object QuoteMatching:
  // @main
  def main(args: String*): Unit =
    quoteMatching(Some(4))
    quoteMatching(List(1, 2, 3).map(_.toHexString).map(_.length))