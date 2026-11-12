package course.macros

import scala.quoted.*

trait TypeClass[A]:
  def message: String

inline def summoning[A <: Tuple]: String =
  ${ summoningImpl[A] }

def summoningImpl[A <: Tuple : Type](using q: Quotes): Expr[String] =
  import q.reflect.*

  Type.of[A] match
    case '[(_, a, _)] =>
      val typeClassOption = Expr.summon[TypeClass[a]]
      val typeClass = typeClassOption.getOrElse:
        report.errorAndAbort(s"missing type class defenition for ${Type.show[a]}")
      '{ $typeClass.message }