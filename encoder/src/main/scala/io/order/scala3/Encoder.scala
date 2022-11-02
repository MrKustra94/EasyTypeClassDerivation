package io.order.scala3

import scala.CanEqual.derived
import scala.deriving.Mirror

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


  given Encoder[String] with
    def encode(s: String): Json = Json.JsonString(s)

  given Encoder[Boolean] with
    def encode(b: Boolean): Json = Json.JsonBoolean(b)

  given Encoder[Long] with 
    def encode(l: Long): Json = Json.JsonFloat(l)

  given seqEncoder[A](using A: Encoder[A]): Encoder[Seq[A]] with
    def encode(s: Seq[A]): Json = Json.JsonValues(s.map(_.asJson()))

  given optionEncoder[A](using A: Encoder[A]): Encoder[Option[A]] with
    def encode(oa: Option[A]): Json = oa.fold(Json.JsonNull)(_.asJson())

  inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = ???

trait EasyWay:
        