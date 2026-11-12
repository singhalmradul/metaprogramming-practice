package course.macros_usage

import course.macros.*
import scala.annotation.targetName


object Symbols:

  enum Permissions:
    case Read(bitset: Int, dir: String, mask: Boolean)
    case Denied

    def changePermissions(bitset: Int, dir: String): String = s"changed $dir permissions to $bitset"

    @targetName("changePermissionsCurried")
    private def changePermissions(bitset: Int)( dir: String): String = s"changed $dir permissions to $bitset"

    val bitMask = 0xFF
  // @main
  def main(args: String*): Unit =
    describeSymbols[Permissions]