package io.order

import io.order.scala3.*

enum OrderEvent derives scala3.Encoder:
  case OrderPlaced(clientId: String, orderItemsCount: Long, prepaid: Boolean)
  case OrderAccepted(clientId: String, orderItemsCount: Long, approver: Option[String])

object Test:
  @main def run = 
    println(OrderEvent.OrderPlaced("XD", 1000, true).asJson())          