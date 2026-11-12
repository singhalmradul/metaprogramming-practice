package course.macros_usage

import course.macros.*

object BuildingExpressions:
  def main(args: String*): Any =
    buildPermissions(Permissions.Custom(List("photos", "books")))
