package course.typesafejdbc

import scala.reflect.Selectable.reflectiveSelectable


object QueryMagic:
  inline def run(inline query: String): List[?] =
    val decoder = QueryResultDecoder.make(query)
    val rows = JdbcCommunication.runQuery(query)
    rows.map(decoder.decode)


object Playground:

  inline val query = "SELECT * FROM users"

  val schema = JdbcCommunication.getSchema(query)

  val idMapping = summon[ColumnMapping["id", JdbcType.TL.Integer, JdbcNullability.TL.NonNullable]]

  type RefinedResult = QueryResult:
    val id: idMapping.Result

  // val magicResult = QueryMagic.run(query)

  // val ids = magicResult.map(_.id)

  def getValues[C <: String, T <: JdbcType.TL, N <: JdbcNullability.TL](rows: List[Row], colName: String)(using mapper: ColumnMapping[C, T, N]): List[mapper.Result] =
    rows.map(_.values).map(_(colName)).map(mapper.reader.read(_))


  // @main
  def main(args: String*): Unit = ???
    // val schema = JdbcCommunication.getSchema(query)
    // println(schema)

    // val rows = JdbcCommunication.runQuery(query)
    // rows.foreach(println)
    // val names = getValues["name", JdbcType.TL.Varchar, JdbcNullability.TL.NonNullable](rows, "name")
    // names.foreach(println)
    // val ages = getValues["age", JdbcType.TL.Integer, JdbcNullability.TL.Nullable](rows, "age")
    // ages.foreach(println)
    // val hobbies = getValues["hobbies", JdbcType.TL.Array[JdbcType.TL.Varchar], JdbcNullability.TL.NonNullable](rows, "hobbies")
    // hobbies.foreach(println)

    // val decoder = QueryResultDecoder.make(query)
    // val rows = JdbcCommunication.runQuery(query)
    // val typedRows = rows.map(decoder.decode)
    // val names = typedRows.map(_.name)
    // names.foreach(println)

    // val typedRows = QueryMagic.run(query)
    // typedRows.map(_.name).foreach(println)
