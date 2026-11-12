package course.macros_usage

import course.macros.*

given TypeClass[String] with
  override def message: String = "string"

object Summoning:
  // @main
  def main(args: String*): Unit =
    summoning[(Int, String, Boolean)]