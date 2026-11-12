package course.typename

import scala.quoted.*
import scala.util.Try


// inline def getTypeName[A]: String = ${ getTypeNameImpl[A] }

// def getTypeNameImpl[A: Type](using Quotes): Expr[String] =
//   Expr(Type.show[A])

// @main
def main(args: String*): Unit =
  val typename = TypeName[List[Option[Try[String]]]]
  println(typename.value)