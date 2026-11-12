package course.macros_usage

import course.macros.*

object StructuralTypes:
  // @main
  def main(args: String*): Unit =
    val person = Record.make(
      "name" -> "John",
      "age" -> 45,
      "favouriteLanguage" -> "Scala"
    )
    val name = person.name
    val age = person.age
    val favouriteLanguage = person.favouriteLanguage

    println(person)