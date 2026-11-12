package course.typesafejdbc

import scala.quoted.*
import ColumnMapping.Aux
import JdbcType.VL


trait QueryResultDecoder[A]:
  def decode(row: Row): A

object QueryResultDecoder:
  transparent inline def make(query: String): QueryResultDecoder[?] =
    ${ makeImpl('query) }

  private def makeImpl(query: Expr[String])(using q: Quotes): Expr[QueryResultDecoder[?]] =
    val schema = JdbcCommunication.getSchema(query.valueOrAbort)

    val descriptor2mappings = schema.values.map(descriptor2mapping)

    val refinedType = makeRefinedType(descriptor2mappings)

    val columReaders = getColumnReaders(descriptor2mappings)

    makeDecoder(columReaders, refinedType)

  case class Descriptor2mapping(descriptor: ColumnDescriptor, mapping: Expr[ColumnMapping[?, ?, ?]])

  private def descriptor2mapping(descriptor: ColumnDescriptor)(using Quotes): Descriptor2mapping =

    val colName = toType(descriptor.name)
    val jdbcType = toType(descriptor.jdbcType)
    val jdbcNullabilty = toType(descriptor.jdbcNullabilty)

    val mapping = (colName, jdbcType, jdbcNullabilty) match
      case (
        '[type c <: String; `c`],
        '[type t <: JdbcType.TL; `t`],
        '[type n <: JdbcNullability.TL; `n`],
      ) => Expr.summon[ColumnMapping[c, t, n]].getOrElse(ColumnMapping.produceColumnMappingError[c, t, n])
      case _ => ???

    Descriptor2mapping(descriptor, mapping)

  private def makeRefinedType(descriptors: List[Descriptor2mapping])(using q: Quotes): Type[?] =
    import q.reflect. *

    descriptors.foldLeft(TypeRepr.of[QueryResult]):
      case (parent, d2m) =>
        val name = d2m.descriptor.name
        val typeRepr = d2m.mapping match
          case '{ $mapping: Aux[_, _, _, r] } => TypeRepr.of[r]

        Refinement(parent, name, typeRepr)
    .asType

  private def getColumnReaders(descriptors: List[Descriptor2mapping])(using Quotes): Expr[List[(String, JdbcReader[?])]] =

    val mapperExprs = descriptors.map: d2m =>
      val name = Expr(d2m.descriptor.name)
      d2m.mapping match
        case '{ $mapping: Aux[_, _, _, r] } => '{ $name -> $mapping.reader }

    Expr.ofList(mapperExprs)

  private def makeDecoder(columnReaders: Expr[List[(String, JdbcReader[?])]], refinedType: Type[?])(using Quotes): Expr[QueryResultDecoder[?]] =
    refinedType match
      case '[r] =>
        '{
          new QueryResultDecoder[r]:
            override def decode(row: Row): r =
              QueryResult($columnReaders)(row).asInstanceOf[r]
        }


  private def toType(vlType: String)(using q: Quotes): Type[?] =
    import q.reflect.*

    ConstantType(StringConstant(vlType)).asType


  private def toType(vlType: JdbcType.VL)(using Quotes): Type[? <: JdbcType.TL] =
    vlType match
      case JdbcType.VL.Boolean => Type.of[JdbcType.TL.Boolean]
      case JdbcType.VL.Integer => Type.of[JdbcType.TL.Integer]
      case JdbcType.VL.Varchar => Type.of[JdbcType.TL.Varchar]
      case JdbcType.VL.Array(content) =>
        val inner = toType(content)
        inner match
          case '[type a <: JdbcType.TL; `a`] => Type.of[JdbcType.TL.Array[a]]
      case JdbcType.VL.NotSupported(_) => Type.of[JdbcType.TL.NotSupported]


  private def toType(vlType: JdbcNullability.VL)(using Quotes): Type[? <: JdbcNullability.TL] =
    vlType match
      case JdbcNullability.VL.NonNullable => Type.of[JdbcNullability.TL.NonNullable]
      case JdbcNullability.VL.Nullable => Type.of[JdbcNullability.TL.Nullable]