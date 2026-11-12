package course.typesafejdbc

trait JdbcReader[A]:
  self =>
  def read(value: Any): A
  def toOption: JdbcReader[Option[A]] = (value: Any) => Option(self.read(value))
  def toList: JdbcReader[List[A]] = (value: Any) => value.asInstanceOf[Array[Any]].toList.map(self.read(_))



object JdbcReader:
  def int: JdbcReader[Int] = (value: Any) => value.asInstanceOf[Int]
  def boolean: JdbcReader[Boolean] = (value: Any) => value.asInstanceOf[Boolean]
  def string: JdbcReader[String] = (value: Any) => value.asInstanceOf[String]