package course.typesafejdbc

object JdbcNullability:
  enum VL:
    case Nullable, NonNullable

  sealed trait TL

  object TL:
    sealed trait Nullable extends TL
    sealed trait NonNullable extends TL