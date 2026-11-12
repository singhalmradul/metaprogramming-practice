package course.macros

import scala.quoted.*

inline def describeSymbols[A]: Unit =
  ${ describeSymbolsImpl[A] }

def describeSymbolsImpl[A: Type](using q: Quotes): Expr[Unit] =
  import q.reflect.*

  val `type` = TypeRepr.of[A]
  val typeSymbol = `type`.typeSymbol
  val method = typeSymbol.methodMember("changePermissions").head
  val field = typeSymbol.fieldMember("bitMask")

  val descriptions = List(
    typeSymbol.fullName,
    typeSymbol.companionModule,
    method.paramSymss,
    method.flags.show,
    method.isDefDef,
    field.pos,
    typeSymbol.children.head
      .primaryConstructor
      .paramSymss,
    typeSymbol.children.head
      .caseFields,
    method.getAnnotation(Symbol.classSymbol("scala.annotation.targetName")),
    typeSymbol.owner,
    typeSymbol.owner.owner,
  )

  println("=====================================debug start==========================================")

  descriptions.foreach(println)

  println("======================================debug end===========================================")

  '{ () }