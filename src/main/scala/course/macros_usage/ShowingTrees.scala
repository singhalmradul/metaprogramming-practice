package course.macros_usage

import course.macros.*

object ShowingTrees:
  // @main
  def main(args: String*): Unit =
    debugExpr(List(1, 2, 3).map(_.toString))
    debugExpr[Either[List[String], Int]](Right(3))
    val x = 3
    debugExpr(s"Scala is awesome :$x")