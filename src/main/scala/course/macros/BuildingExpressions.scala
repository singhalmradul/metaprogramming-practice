package course.macros

import scala.quoted.*

enum Permissions:
  case Denied
  case Bitset(value: Int)
  case Custom(dirs: List[String])


given ToExpr[Permissions] with
  import Permissions.*
  override def apply(x: Permissions)(using Quotes): Expr[Permissions] = x match
    case Denied => '{ Denied }
    case Bitset(value) =>
      val valueExpr = Expr(value)
      '{ Bitset( $valueExpr ) }
    case Custom(dirs) =>
      val dirsExpr = Expr(dirs)
      '{Custom( $dirsExpr )}

given FromExpr[Permissions] with
  import Permissions.*
  override def unapply(x: Expr[Permissions])(using Quotes): Option[Permissions] = x match
    case '{ Denied } => Some(Denied)
    case '{ Bitset($valueExpr) } =>
      val value = valueExpr.valueOrAbort
      Some(Bitset(value))
    case '{ Custom($dirsExpr) } =>
      val dirs = dirsExpr.valueOrAbort
      Some(Custom(dirs))
    case _ => Some(Denied)


def buildPermissionExpr(using Quotes): Expr[Permissions] =
  Expr(Permissions.Custom(List("photos", "books")))

inline def buildPermissions(inline permissions: Permissions): String =
  ${ buildPermissionsImpl('permissions) }

def buildPermissionsImpl(permsisions: Expr[Permissions])(using Quotes): Expr[String] =
  import Permissions.*
  val p = permsisions.valueOrAbort
  val result = p match
    case Denied => "no access"
    case Bitset(value) => s"permissions with $value"
    case Custom(dirs) => s"permssions to directories ${dirs.mkString(", ")}"

  Expr(result)