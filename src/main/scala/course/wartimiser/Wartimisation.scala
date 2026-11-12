package course.wartimiser

import scala.quoted.Quotes
import scala.quoted.FromExpr
import scala.quoted.Expr
import scala.reflect.NameTransformer

trait Wartimisation:
  self: Singleton =>

  def treeMap(using q: Quotes): q.reflect.TreeMap

object Wartimisation:
  given FromExpr[Wartimisation] with
    override def unapply(x: Expr[Wartimisation])(using
        q: Quotes
    ): Option[Wartimisation] =
      import q.reflect.*

      val typeSymbol = x.asTerm.tpe.typeSymbol

      if typeSymbol.flags.is(Flags.Module) then
        val fullName = typeSymbol.fullName
        Some(unsafeLoadObject(fullName))
      else
        report.errorAndAbort(
          s"The type [${typeSymbol.name}] doesn't correspond to an object",
          x
        )

    private def unsafeLoadObject[A](name: String)(using q: Quotes): A =
      import q.reflect.*

      try
        val clazz = Class.forName(name)
        val module = clazz.getField(NameTransformer.MODULE_INSTANCE_NAME)
        val objectInstance = module.get(null)
        objectInstance.asInstanceOf[A]
      catch
        case e =>
          report.errorAndAbort(s"""|
          |Failed to load class [$name]
          |Make sure that:
          | - it is a top-level object (or nested in other object)
          | - it is defined in a separate file from where the method is being invoked
          | - it being referred to directly as object rather than alias or val
          ${e.getMessage}
          """")
