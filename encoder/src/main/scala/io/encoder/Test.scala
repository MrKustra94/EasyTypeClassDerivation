package io.encoder

import io.encoder.scala3.*

import scala.deriving.*
import scala.quoted.Expr

object Ids:
  opaque type ClientId = String

  object ClientId:
    def make(str: String): ClientId = str

    given Encoder[ClientId] = Encoder.strEncoder

enum OrderEvent:
  case OrderPlaced(clientId: Ids.ClientId, orderItemsCount: Int, prepaid: Boolean)
  case OrderAccepted(clientId: Ids.ClientId, orderItemsCount: Int, approver: Option[String])
  case OrderRejected(clientId: Ids.ClientId, canBeResumed: Boolean)

object OrderEvent:
  given Encoder[OrderEvent] = Encoder.derived[OrderEvent]

object Test:
  @main def run =
    val or: OrderEvent = OrderEvent.OrderPlaced(Ids.ClientId.make("Customer"), 1000, true)
    var json: Json = null
    while (true) {
      val start = System.nanoTime()
      json = or.asJson()
      val end = System.nanoTime()
      println(s"Took: ${end - start} ns.")
    }
    println(json.prettyString())
      