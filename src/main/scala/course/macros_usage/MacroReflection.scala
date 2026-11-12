package course.macros_usage

import course.macros.*

case class Container(a: Int):
  def store(b: Int): String = s"storing $a and $b"

object MacroReflection:
  // @main
  def main(args: String*): Unit =
    val a = 32
    invokeMethodDynamically(Container(5), a, "store")
    val tuple = createTuple[3, String]("Scala")
    println(tuple)

    val tuple1 = createTuple1[3, String]("Scala")
    println(tuple1)