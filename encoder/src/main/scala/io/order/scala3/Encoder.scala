package io.order.scala3

import scala.CanEqual.derived
import scala.deriving.Mirror
import scala.compiletime.*

enum Json:
    case JsonNull
    case JsonBoolean(value: Boolean)
    case JsonInt(value: Long)
    case JsonFloat(value: Double)
    case JsonString(value: String)
    case JsonValues(value: Seq[Json])
    case JsonObject(value: Map[String, Json])

trait Encoder[A]:
  def encode(a: A): Json
  
  extension(a: A)
    def asJson(): Json = encode(a)


object Encoder extends EasyWay:    
  given Encoder[String] with
    def encode(s: String): Json = Json.JsonString(s)

  given Encoder[Boolean] with
    def encode(b: Boolean): Json = Json.JsonBoolean(b)

  given Encoder[Long] with 
    def encode(l: Long): Json = Json.JsonInt(l)

  given seqEncoder[A](using A: Encoder[A]): Encoder[Seq[A]] with
    def encode(s: Seq[A]): Json = Json.JsonValues(s.map(_.asJson()))

  given optionEncoder[A](using A: Encoder[A]): Encoder[Option[A]] with
    def encode(oa: Option[A]): Json = oa.fold(Json.JsonNull)(_.asJson())

  inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = derivedEasy[T]

private[scala3] trait EasyWay:
  inline def derivedEasy[T](using m: Mirror.Of[T]): Encoder[T] = 
    inline m match
      case s: Mirror.SumOf[T] => 
        val childrenEncoders = deriveEncoders[s.MirroredElemTypes]
        encoderSum(s, childrenEncoders)
      case p: Mirror.ProductOf[T] => 
        val fieldsEncoders = summonEncoders[p.MirroredElemTypes]
        val fieldsNames = constValueTuple[p.MirroredElemLabels].toList.asInstanceOf[List[String]]
        encoderProduct(p, fieldsEncoders, fieldsNames)
    
  inline final def deriveEncoders[T <: Tuple]: IndexedSeq[Encoder[_]] = 
    inline erasedValue[T] match
      case _: EmptyTuple => Vector.empty
      case _: (t *: ts) => deriveEncoder[t] +: deriveEncoders[ts]

  inline final def deriveEncoder[A]: Encoder[A] = 
    summonFrom { 
        case encodeA: Encoder[A] => encodeA
        case _: Mirror.Of[A] => derivedEasy[A]
    }  

  inline final def encoderSum[T](m: Mirror.SumOf[T], children: IndexedSeq[Encoder[_]]): Encoder[T] = 
    new Encoder[T]:
      def encode(t: T): Json = 
        children(m.ordinal(t)).asInstanceOf[Encoder[T]].encode(t)

  inline def summonEncoders[T <: Tuple]: List[Encoder[_]] =
     inline erasedValue[T] match 
       case _: EmptyTuple => Nil
       case _: (t *: ts) => summonInline[Encoder[t]] :: summonEncoders[ts]

  inline final def encoderProduct[T](
    s: Mirror.ProductOf[T], 
    fieldsEncoders: List[Encoder[_]],
    fieldsNames: List[String]
  ): Encoder[T] = 
    new Encoder[T]:
      private val encoders = fieldsEncoders
      private val fields = fieldsNames

      def encode(t: T): Json =   
        val zippedProps = t.asInstanceOf[scala.Product].productIterator.zip(fields).zip(encoders)
        val jsonFields = zippedProps.map { case ((value, name), encoder) =>
            name -> encoder.asInstanceOf[Encoder[Any]].encode(value)
        }
        Json.JsonObject(jsonFields.toMap)