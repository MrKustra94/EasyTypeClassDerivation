package io.encoder.scala2

sealed trait Json

case object Json {
    case object JsonNull extends Json
    final case class JsonBoolean(value: Boolean) extends Json
    final case class JsonInt(value: Long) extends Json
    final case class JsonFloat(value: Double) extends Json
    final case class JsonString(value: String) extends Json
    final case class JsonValues(value: Seq[Json]) extends Json
    final case class JsonObject(value: Map[String, Json]) extends Json
}

// Type class definition
trait Encoder[T] {
    def encode(t: T): Json
}

object Encoder {
    // Method used to summon type class instance for given type.
    // Example usage: 
    // val json = Encoder[Int].encode(100)
    def apply[T: Encoder]: Encoder[T] = implicitly

    // Implicit class are used to provide extension methods.
    // They provide convenient, dot syntax API 
    implicit class EncoderOps[T](t: T)(implicit T: Encoder[T]){
       def asJson(): Json = T.encode(t)
    }

    implicit val booleanEncoder: Encoder[Boolean] = 
        (t: Boolean) => Json.JsonBoolean(t)

    implicit val stringEncoder: Encoder[String] = new Encoder[String]{
        def encode(t: String): Json = Json.JsonString(t)
    }

    implicit val intEncoder: Encoder[Int] = new Encoder[Int]{
        def encode(t: Int): Json = Json.JsonInt(t)
    }

    implicit def seqEncoder[T: Encoder]: Encoder[Seq[T]] = 
        (ts: Seq[T]) => Json.JsonValues(ts.map(_.asJson()))

    implicit def optionEncoder[T: Encoder]: Encoder[Option[T]] =
        (to: Option[T]) => to.fold(Json.JsonNull)(_.asJson()) 
}


