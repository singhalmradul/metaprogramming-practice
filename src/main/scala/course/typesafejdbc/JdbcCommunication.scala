package course.typesafejdbc

import java.sql.DriverManager
import java.sql.Connection
import java.sql.ResultSet
import scala.collection.mutable.ListBuffer

object JdbcCommunication:
  private def withConnection[A](f: Connection => A): A =
    Class.forName("org.postgresql.Driver")
    val connection = DriverManager.getConnection("jdbc:postgresql://localhost:5432/temp", "postgres", "postgres")
    try
      f(connection)
    finally
      connection.close()

  private def parseRows(schema: Schema, resultSet: ResultSet): List[Row] =
    val result = ListBuffer.empty[Row]

    while resultSet.next() do
      val row = schema.values.map: descriptor =>
        val value = resultSet.getObject(descriptor.index) match
          case array: java.sql.Array => array.getArray
          case obj => obj
        descriptor.name -> value

      result += Row(row.toMap)

    result.toList

  def getSchema(query: String): Schema =
    withConnection: connection =>
      val statement = connection.prepareStatement(query)
      val metadata = statement.getMetaData
      Schema.fromMetadata(metadata)

  def runQuery(query: String): List[Row] =
    withConnection: connection =>
      val statement = connection.prepareStatement(query)
      val result = statement.executeQuery()
      val metadata = statement.getMetaData
      val schema = Schema.fromMetadata(metadata)

      parseRows(schema, result)