package course.typesafejdbc

object JdbcType:
  enum VL:
    case Integer, Boolean, Varchar
    case Array(`type`: VL)
    case NotSupported(message: String)

  sealed trait TL

  object TL:
    sealed trait Integer extends TL
    sealed trait Boolean extends TL
    sealed trait Varchar extends TL
    sealed trait Array[T <: TL] extends TL
    sealed trait NotSupported extends TL