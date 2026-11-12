package course.typesafejdbc

import java.sql.ResultSetMetaData
import java.sql.Types

case class ColumnDescriptor(
    index: Int,
    name: String,
    jdbcType: JdbcType.VL,
    jdbcNullabilty: JdbcNullability.VL
)

final case class Schema(values: List[ColumnDescriptor])

object Schema:
  def fromMetadata(metadata: ResultSetMetaData): Schema =
    val descriptors =
      for f <- 1 to metadata.getColumnCount
      yield ColumnDescriptor(
        index = f,
        name = metadata.getColumnName(f),
        jdbcType = getJdbcType(metadata, f),
        jdbcNullabilty = getJdbcTNullability(metadata, f)
      )

    Schema(descriptors.toList)

  private def getJdbcType(metadata: ResultSetMetaData, f: Int): JdbcType.VL =
    metadata.getColumnType(f) match
      case Types.INTEGER => JdbcType.VL.Integer
      case Types.BOOLEAN => JdbcType.VL.Boolean
      case Types.VARCHAR => JdbcType.VL.Varchar
      case Types.ARRAY if metadata.getColumnTypeName(f).contains("varchar") =>
        JdbcType.VL.Array(JdbcType.VL.Varchar)
      case _ =>
        JdbcType.VL.NotSupported(metadata.getColumnTypeName(f))

  private def getJdbcTNullability(
      metadata: ResultSetMetaData,
      f: Int
  ): JdbcNullability.VL =
    metadata.isNullable(f) match
      case ResultSetMetaData.columnNoNulls => JdbcNullability.VL.NonNullable
      case _                               => JdbcNullability.VL.Nullable
