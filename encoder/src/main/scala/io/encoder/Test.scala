package io.encoder

import io.encoder.scala3.*

import scala.deriving.*
import scala.quoted.Expr
import scala.collection.mutable.ListBuffer
import io.encoder.Ids.ClientId

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
  //Hand written naive:
  //Mean: 208
  //99p: 417

  // Hand written optimized (with PM):
  // Mean: 83
  // 99p: 250
  given Encoder[OrderEvent] with
    def encode(oe: OrderEvent): Json = 
      oe match
        case ord: OrderPlaced =>
          val fields = Map.empty[String, Json]
          Json.JsonObject(
            fields + 
              (("type", Encoder[String].encode("OrderPlaced"))) +
              (("clientId", Encoder[ClientId].encode(ord.clientId))) +
              (("orderitemsCound", Encoder[Int].encode(ord.orderItemsCount))) +
              (("prepaid", Encoder[Boolean].encode(ord.prepaid)))
          )
        case _ => Json.JsonBoolean(false)    

object Test:
  @main def run =
    val oe: OrderEvent = OrderEvent.OrderPlaced(Ids.ClientId.make("Customer"), 1000, true)
    println(oe.asJson().prettyString())
      