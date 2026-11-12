package course.macros

import scala.quoted.*
import scala.annotation.transparentTrait


inline def invokeMethodDynamically[A](inline instance: A, inline arg: Int, inline methodName: String): String =
  ${ invokeMethodDynamicallyImpl('instance, 'arg, 'methodName) }

def invokeMethodDynamicallyImpl[A](instanceExpr: Expr[A], arg: Expr[Int], methodName: Expr[String])(using q: Quotes): Expr[String] =
  import q.reflect.*
  val instance = instanceExpr.asTerm
  val method = Select.unique(instance, methodName.valueOrAbort)
  val invoked = Apply(method, List(arg.asTerm))
  invoked.asExprOf[String]

transparent inline def createTuple[N <: Int, A](inline value: A): Tuple =
  ${ createTupleImpl[N, A]('value) }

def createTupleImpl[N <: Int: Type, A: Type](value: Expr[A])(using q: Quotes): Expr[Tuple] =
  import q.reflect.*

  /* transparent inline */ def createTupleAux(x: Int): Expr[Tuple] =
    if x == 0 then '{ EmptyTuple }
    else
      val pre = x - 1
      '{ $value *: ${ createTupleAux(pre) } }

  TypeRepr.of[N] match
    case ConstantType(IntConstant(x)) if x >= 0 => createTupleAux(x)
    case _ => report.errorAndAbort(s"invalid tuple size ${Type.show[N]}")


transparent inline def createTuple1[N <: Int, A](inline value: A): Tuple =
  ${ createTuple1Impl[N, A]('value) }

def createTuple1Impl[N <: Int: Type, A: Type](value: Expr[A])(using q: Quotes): Expr[Tuple] =
  import q.reflect.*

  def buildTupleExpr(`type`: Type[? <: AnyKind]): Expr[Tuple] = `type` match
    case '[_ *: rt] => '{ $value *: ${ buildTupleExpr(TypeRepr.of[rt].asType) } }
    case _ => '{ EmptyTuple }

  transparent inline def createTupleAux(x: Int): Expr[Tuple] =
    val typeConstructor = defn.TupleClass(x).typeRef
    val typeArguments = List.fill(x)(TypeRepr.of[A])
    val appliedType = AppliedType(typeConstructor, typeArguments)
    val actualType = appliedType.asType

    buildTupleExpr(actualType)

  TypeRepr.of[N] match
    case ConstantType(IntConstant(x)) if x >= 0 => createTupleAux(x)
    case _ => report.errorAndAbort(s" invalid tuple size ${Type.show[N]}")
