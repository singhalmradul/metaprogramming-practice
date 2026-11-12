import scala.compiletime.*
import scala.compiletime.ops.int.*
import scala.compiletime.ops.any.ToString
import scala.deriving.Mirror
import scala.quoted.*
import scala.util.Try

def inc(x: Int) = x + 1
inline def inc1(x: Int): Int = x + 1
inline def inc2(inline x: Int) = x + 1
transparent inline def wrap[A](x: A): Option[A] = Some(x)
inline def condition(b: Boolean): String = inline if b then "yes" else "no"
transparent inline def sum(x: Int): Int = inline if x <= 0 then 0 else x + sum(x - 1)
inline def matcher[N <: Int]: String = inline erasedValue[N] match
  case _: 1 => "one"
  case _: 2 => "two"
  case _: 3 => "three"
  case _ => compiletime.error("invalid value: " + constValue[ToString[N]])

trait Semigroup[A]:
  def combine(a: A, b: A): A

def double[A : Semigroup](a: A): A =
  summon[Semigroup[A]].combine(a, a)

transparent inline def double1[A](a: A): A =
  summonInline[Semigroup[A]].combine(a, a)

trait Messenger[A]:
  def message: String

given Messenger[Int] with
  def message: String = "this is an int speaking"

inline def produceMessage[A]: String =
  summonFrom:
    case x: Messenger[A] => "Found messager: " + x.message
    case _ => "Bummer, no messenger found for this type"


transparent inline def factorial[N <: Int]: Int =
  inline erasedValue[N] match
    case _: 0 => 1
    case _: S[n] => constValue[n + 1] * factorial[n]

@FunctionalInterface
trait Show[A]:
  def show(a: A): String

object Show:
  given Show[Int] = _.toString
  given Show[String] = identity
  given Show[Boolean] = _.toString

  private inline def showTuple[E <: Tuple, L <: Tuple](elements: E): List[String] =
    inline (elements, erasedValue[L]) match
      case (EmptyTuple, EmptyTuple) => List.empty
      case (el: (eh *: et), lb: (lh *: lt)) =>
        val h *: t = el
        constValue[ToString[lh]] + ": " + summonInline[Show[eh]].show(h) :: showTuple[et, lt](t)

  inline def derived[A <: Product](using m: Mirror.ProductOf[A]): Show[A] =
    (a: A) =>
        val values = Tuple.fromProductTyped(a)
        val fieldReps = showTuple[m.MirroredElemTypes, m.MirroredElemLabels](values)
        fieldReps.mkString("{\n\t", ",\n\t", "\n}")



transparent inline def showTuple[T <: Tuple](tuple: T): String = inline tuple match
  case _: EmptyTuple => ""
  case tup: (h *: t) =>
    val head *: tail = tup
    summonInline[Show[h]].show(head) + " " + showTuple[t](tail)

case class Person(name: String, age: Int, programmer: Boolean) derives Show


// @main
def main(args: String*): Unit =
  val x = 3
  val four = inc(x)
  val four1 = inc1(x)
  val eight = inc1(2 * x + 1)
  val eight1 = inc2(2 * x + 1)
  val intOption: Some[Int] = wrap(5)
  val no = condition(false || !true)
  val one = matcher[1]
  val ten: 10 = sum(4)
  given Semigroup[Int] = _ + _
  val six = double(3)
  val two = double1(1)
  val intMessage = produceMessage[Int]
  val otherMessage = produceMessage[String]
  // val error = matcher[3 + 2](5)
  val twentyFour: 24 = factorial[4]
  val tupleStr = showTuple(("Scala", true, 42))
  println(tupleStr)
  val personStr = summon[Show[Person]].show(Person("madz1", 11, true))
  println(personStr)
  val personStr1 = Person.derived$Show.show(Person("madz", 12, false))
  println(personStr1)