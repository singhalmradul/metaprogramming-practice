package course.macros_usage

import course.macros.*

object TreeMappings:
  @main
  def main(args: String*): Unit =
    val scopedValue = transformCode:
      def add(a: Int, b: Int): Int = a + b
      val x = 42
      val str = "Scala"
      println(s"x=$x, str=$str")
      println(add(1, 2))

    val flippedBooleans = flipBooleans:
      val a = true
      val b = 2 > 3
      val c = a && b
      def funcBool(a: Boolean, b: Boolean): Boolean = a && b
      if (c || false) funcBool(a, b)
      else false

    val gatheredStatements = treeAccumulator:
      val a = 1 + 3

      println(a)

      val b =
        println("scala")
        a * 3

      def log(a: Int, b: Int): Int =
        println(a)
        println(b)
        a + b

      println(log(a, b))