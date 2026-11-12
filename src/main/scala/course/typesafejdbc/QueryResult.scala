package course.typesafejdbc

class QueryResult(colReaders: List[(String, JdbcReader[?])])(row: Row) extends Selectable:
  val readerMap = colReaders.toMap

  def selectDynamic(name: String): Any =
    val reader = readerMap(name)
    val value = row.values(name)
    reader.read(value)