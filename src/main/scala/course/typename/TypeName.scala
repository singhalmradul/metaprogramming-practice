package course.typename

import scala.quoted.*
import scala.util.Try

case class TypeName[A](value: String)

// object TypeName:
//   def apply[A]: TypeName[A] =
//     val typeName = getTypeName[A]
//     TypeName[A](typeName)


object TypeName:
  def apply[A: TypeName]: TypeName[A] = summon[TypeName[A]]

  inline given getTypeName[A]: TypeName[A] = ${ getTypeNameImpl[A] }


  private def getTypeNameImpl[A: Type](using Quotes): Expr[TypeName[A]] =
    val typeName = Expr(Type.show[A])
    '{ TypeName[A]($typeName) }

// @main
// def main(args: String*): Unit =
//   val typename = TypeName[List[Option[Try[String]]]]
//   println(typename.value)