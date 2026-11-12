package course.typesafejdbc

import scala.quoted.*

trait ColumnMapping[C <: String, T <: JdbcType.TL, N <: JdbcNullability.TL]:
  type Result

  def reader: JdbcReader[Result]

object ColumnMapping:
  import JdbcNullability.TL.*

  type Aux[C <: String, T <: JdbcType.TL, N <: JdbcNullability.TL, R] =
    ColumnMapping[C, T, N]:
      type Result = R

  given [C <: String]: ColumnMapping[C, JdbcType.TL.Integer, NonNullable] with
    type Result = Int
    override def reader: JdbcReader[Result] = JdbcReader.int

  given [C <: String]: ColumnMapping[C, JdbcType.TL.Boolean, NonNullable] with
    type Result = Boolean
    override def reader: JdbcReader[Result] = JdbcReader.boolean

  given [C <: String]: ColumnMapping[C, JdbcType.TL.Varchar, NonNullable] with
    type Result = String
    override def reader: JdbcReader[Result] = JdbcReader.string

  given [C <: String, T <: JdbcType.TL, R](using
      existingMapper: Aux[C, T, NonNullable, R]
  ): ColumnMapping[C, T, Nullable] with
    type Result = Option[R]
    override def reader: JdbcReader[Result] = existingMapper.reader.toOption

  given [C <: String, T <: JdbcType.TL, R](using
      existingMapper: Aux[C, T, NonNullable, R]
  ): ColumnMapping[C, JdbcType.TL.Array[T], NonNullable] with
    type Result = List[R]
    override def reader: JdbcReader[Result] = existingMapper.reader.toList


  def produceColumnMappingError[C: Type, T: Type, N: Type](using q: Quotes) =
    import q.reflect.*

    given Printer[TypeRepr] = Printer.TypeReprShortCode
    val cType = TypeRepr.of[C].show
    val tType = TypeRepr.of[T].show
    val nType = TypeRepr.of[N].show

    report.errorAndAbort(s"failed to summon ColumnMapping[$cType, $tType, $nType]")