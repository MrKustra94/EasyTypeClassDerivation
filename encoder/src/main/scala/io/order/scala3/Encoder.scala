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

  inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = derivedEasy

private[scala3] trait EasyWay:
  inline def derivedEasy[T](using m: Mirror.Of[T]): Encoder[T] = 
    inline m match
      case p: Mirror.ProductOf[T] => 
        encoderProduct(p)  
      case s: Mirror.SumOf[T] => 
        val childrenEncoders = deriveEncoders[s.MirroredElemTypes]
        encoderSum(s, childrenEncoders)
    
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

  inline final def encoderProduct[T](s: Mirror.ProductOf[T]): Encoder[T] = 
    new Encoder[T]:
      def encode(t: T): Json =   
        Json.JsonObject(go[s.MirroredElemTypes, s.MirroredElemLabels](t.asInstanceOf[Product], Map.empty)(0))

  inline def go[T <: Tuple, L <: Tuple](instance: Product, acc: Map[String, Json])(index: Int): Map[String, Json] =
    inline erasedValue[(T, L)] match
      case (_: (t *: ts), _: (l *: ls)) => 
        val label = constValue[l].toString
        val encoder = summonInline[Encoder[t]]
        val newAcc = acc + (label -> encoder.encode(instance.productElement(index).asInstanceOf[t]))
        go[ts, ls](instance, newAcc)(index + 1)
      case (_, _) => acc