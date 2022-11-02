package io.order.scala2

import math.Numeric.Implicits.infixNumericOps

sealed trait Ordering

object Ordering {
    case object GreaterThan extends Ordering
    case object LessThan extends Ordering
    case object Equal extends Ordering
}

// Type class definition
trait Ord[A] {
  def compare(lhs: A, rhs: A): Ordering
}

object Ord {
    //Syntax sugar for dot syntax
    implicit class OrdOps[A](lhs: A)(implicit ord: Ord[A]){
        def compare(rhs: A): Ordering = ord.compare(lhs, rhs)
    }

    implicit val intOrd: Ord[Int] = 
        new Ord[Int] {
          override def compare(lhs: Int, rhs: Int): Ordering = {
            val cmpResult = lhs - rhs
            if (cmpResult < 0) Ordering.LessThan      
            else if (cmpResult > 0) Ordering.GreaterThan  
            else Ordering.Equal
          }    
        }

    implicit def fromNumericOrd[N](implicit N: Numeric[N]): Ord[N] = 
        new Ord[N] {
          override def compare(lhs: N, rhs: N): Ordering = {
            val cmpRes = N.sign(N.minus(lhs, rhs))
            if (cmpRes == N.negate(N.one)) Ordering.LessThan      
            else if (cmpRes == N.one) Ordering.GreaterThan  
            else Ordering.Equal
          }
        }      
}




