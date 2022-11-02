package io.order

import io.order.scala3.*

import scala.deriving.*
import scala.quoted.Expr

enum OrderEvent derives Encoder:
  case OrderPlaced(clientId: String, orderItemsCount: Long, prepaid: Boolean)
  case OrderAccepted(clientId: String, orderItemsCount: Long, approver: Option[String])
  case OrderRejected(clientId: String, canBeResumed: Boolean)

object Test:
  @main def run = ???
      