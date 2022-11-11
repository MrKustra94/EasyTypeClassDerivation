package io.encoder.scala3

import scala.CanEqual.derived
import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.annotation.tailrec

enum Json:
    case JsonNull
    case JsonBoolean(value: Boolean)
    case JsonInt(value: Long)
    case JsonFloat(value: Double)
    case JsonString(value: String)
    case JsonValues(value: Seq[Json])
    case JsonObject(value: Map[String, Json])

    def prettyString(): String = 
      this match
        case Json.JsonNull => "null"
        case Json.JsonBoolean(value) => value.toString()
        case Json.JsonInt(value) => value.toString()
        case Json.JsonFloat(value) => value.toString()
        case Json.JsonString(value) => s"\"$value\""
        case Json.JsonValues(values) => values.map(_.prettyString()).mkString("[", ",", "]")
        case Json.JsonObject(value) => value.map(e => s"\"${e._1}\": ${e._2.prettyString()}").mkString("{\n  ", ",\n  ", "\n}")

// Type class definition
trait Encoder[A]:
  def encode(a: A): Json
  
  // Extension block defines additional, convenient methods for callers to be used.
  extension(a: A)
    def asJson(): Json = encode(a)

object Encoder:    
  def apply[A: Encoder]: Encoder[A] = summon

  given strEncoder: Encoder[String] with
    def encode(s: String): Json = Json.JsonString(s)

  given Encoder[Boolean] with
    def encode(b: Boolean): Json = Json.JsonBoolean(b)

  given Encoder[Int] with 
    def encode(i: Int): Json = Json.JsonInt(i)

  given seqEncoder[A](using A: Encoder[A]): Encoder[Seq[A]] with
    def encode(s: Seq[A]): Json = Json.JsonValues(s.map(_.asJson()))

  given optionEncoder[A](using A: Encoder[A]): Encoder[Option[A]] with
    def encode(oa: Option[A]): Json = oa.fold(Json.JsonNull)(_.asJson())

  inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = 
    inline m match
      case p: Mirror.ProductOf[T] => 
        derivedP(p, false)
      case s: Mirror.SumOf[T] => 
        val derivedInstances = deriveEncoders[s.MirroredElemTypes]
        new Encoder[T]:
          def encode(t: T): Json = 
            val to = s.ordinal(t)
            derivedInstances(to).asInstanceOf[Encoder[T]].encode(t)

  inline def derivedP[T](p: Mirror.ProductOf[T], withDiscriminator: Boolean): Encoder[T] =
    new Encoder[T]:
      def encode(t: T): Json =
        val initialMap = 
          inline if withDiscriminator then
            val typeJsoned = summonInline[Encoder[String]].encode(constValue[p.MirroredLabel])
            Map.empty[String, Json] + (("type", typeJsoned))
          else Map.empty[String, Json]
        go[p.MirroredElemLabels, p.MirroredElemTypes, T](initialMap, t)

  inline def go[L <: Tuple, T <: Tuple, I](acc: Map[String, Json], instance: I): Json.JsonObject =
    inline erasedValue[(L, T)] match
      case _: (l *: ls, t *: ts) =>        
        go[ls, ts, I](acc + makeJsonEntry[l, t, I](instance), instance)      
      case _ => Json.JsonObject(acc)

  inline def makeJsonEntry[L, T, I](instance: I): (String, Json) = 
    ${ makeJsonEntryMacro[L, T, I]('instance) }

  private def makeJsonEntryMacro[L: Type, T: Type, I](instance: Expr[I])(using Quotes): Expr[(String, Json)] = 
    import quotes.reflect.*
    val fieldName = Type.valueOfConstant[L].getOrElse(report.throwError("XD2")).asInstanceOf[String]
    val encoder = Expr.summon[Encoder[T]].getOrElse(report.throwError("XD"))
    '{
      val fieldNameV = ${Expr(fieldName)}
      val selected = ${Select.unique(instance.asTerm, fieldName).asExprOf[T]}
      val json = $encoder.encode(selected)
      (fieldNameV, json)
    }


  inline def summonEncoders[T <: Tuple]: List[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Encoder[t]] :: summonEncoders[ts]   

  inline def deriveEncoders[T <: Tuple]: IndexedSeq[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Vector.empty
      case _: (t *: ts) => summonOrDerive[t] +: deriveEncoders[ts]

  // Must be defined separately    
  inline def summonOrDerive[T] = 
    summonFrom { 
      case encoder: Encoder[T] => encoder
      case p : Mirror.ProductOf[T] => derivedP(p, true)
    }    