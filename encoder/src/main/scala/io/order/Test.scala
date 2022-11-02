package io.order

import io.order.scala3.*

enum Countdown:
    case Elapsed extends Countdown
    case InProgress(days: Long, hours: Byte, minutes: Byte, seconds: Byte, millis: Short) extends Countdown

object Countdown:
    given Ord[Countdown] with
        def compare(lhs: Countdown, rhs: Countdown): Ordering = 
          (lhs, rhs) match {
            case (Elapsed, Elapsed) => Ordering.Equal
            case (Elapsed, _) => Ordering.LessThan
            case (_, Elapsed) => Ordering.GreaterThan
            case (lhsip: InProgress, rhsip: InProgress) =>
                val daysCmp = summon[Ord[Long]].compare(lhsip.days, rhsip.days)
                if daysCmp != Ordering.Equal then daysCmp
                else
                  val hoursCmp = summon[Ord[Byte]].compare(lhsip.hours, rhsip.hours)
                  if hoursCmp != Ordering.Equal then hoursCmp
                  else
                    val minutesCmp = summon[Ord[Byte]].compare(lhsip.minutes, rhsip.minutes)
                    if minutesCmp != Ordering.Equal then minutesCmp
                    else 
                      val secondsCmp = summon[Ord[Byte]].compare(lhsip.seconds, rhsip.seconds)
                      if secondsCmp != Ordering.Equal then secondsCmp
                      else summon[Ord[Short]].compare(lhsip.millis, rhsip.millis)
          }  

object Test:
    @main def run = 
        val c1 = Countdown.InProgress(100000, 10, 12, 14, 900)
        val c2 = Countdown.InProgress(100000, 10, 12, 14, 900)
        println(c1 === c2)          