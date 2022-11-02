package io.order.scala3

import scala.compiletime.*
import scala.deriving.Mirror
import scala.annotation.tailrec
import math.Numeric.Implicits.infixNumericOps

enum Ordering:
  case Equal, LessThan, GreaterThan

trait Ord[T]:
  def compare(lhs: T, rhs: T): Ordering

  extension(lhs: T)
    def < (rhs: T): Boolean = compare(lhs, rhs) == Ordering.LessThan
    def > (rhs: T): Boolean = compare(lhs, rhs) == Ordering.GreaterThan
    def === (rhs: T): Boolean = compare(lhs, rhs) == Ordering.Equal

object Ord:

  given intOrd: Ord[Int] with
    def compare(lhs: Int, rhs: Int): Ordering = 
      val diff = lhs - rhs
      if diff < 0 then Ordering.LessThan
      else if diff > 0 then Ordering.GreaterThan
      else Ordering.Equal 

  given numericOrd[N](using N: Numeric[N]): Ord[N] with
    def compare(lhs: N, rhs: N): Ordering = 
      val diff = N.sign(N.minus(lhs, rhs))
      if diff == N.negate(N.one) then Ordering.LessThan
      else if diff == N.one then Ordering.GreaterThan
      else Ordering.Equal

  inline def derived[T](using tm: Mirror.Of[T]): Ord[T] = 
    //val elemInstances = summonOrders[tm.MirroredElemTypes]
    //inline tm match
    //  case sum: Mirror.SumOf[T] => ordSum(sum, elemInstances)
    //  case product: Mirror.ProductOf[T] => ordProduct(product, elemInstances)        
    ${ SemiDerive.derived[T] }

  private inline def summonOrders[T <: Tuple]: IndexedSeq[Ord[_]] = 
    inline erasedValue[T] match
      case _: EmptyTuple => Vector.empty
      case _: (t *: ts) => summonOrd[t] +: summonOrders[ts]

  private inline final def summonOrd[A]: Ord[A] = summonFrom { 
    case encodeA: Ord[A] => encodeA
    case _: Mirror.Of[A] => derived[A]
  }

  private inline def ordSum[T](s: Mirror.SumOf[T], children: => IndexedSeq[Ord[_]]): Ord[T] = 
    new Ord[T]:
      def compare(lhs: T, rhs: T): Ordering = 
        val lhsi = s.ordinal(lhs)
        val rhsi = s.ordinal(rhs)
        if lhsi < rhsi then Ordering.LessThan
        else if lhsi > rhsi then Ordering.GreaterThan
        else
          val ord = children(s.ordinal(lhs)).asInstanceOf[Ord[T]]
          ord.compare(lhs, rhs)

  private inline def ordProduct[T](s: Mirror.ProductOf[T], fieldsOrders: => IndexedSeq[Ord[_]]): Ord[T] = 
    new Ord[T]:
      def compare(lhs: T, rhs: T): Ordering =   
        val lhsp = lhs.asInstanceOf[scala.Product]
        val rhsp = rhs.asInstanceOf[scala.Product]

        @tailrec
        def go(orders: IndexedSeq[Ord[_]], index: Int): Ordering =
          orders match
            case h +: t => 
              val casted = h.asInstanceOf[Ord[Any]]
              val lhsf = lhsp.productElement(index)
              val rhsf = rhsp.productElement(index)
              casted.compare(lhsf, rhsf) match
                case Ordering.Equal => 
                  go(t, index + 1)
                case other => other   
            case _ => Ordering.Equal
          end match  

        go(fieldsOrders, 0)  

object SemiDerive:
  import scala.deriving._
  import scala.quoted._

  given derived[T: Type](using Quotes): Expr[Ord[T]] =
    val mirror: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get
    mirror match
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = fieldsTypes; type MirroredElemLabels = fieldsNames } } => 
        import quotes.reflect.*
        val fieldOrds = summonOrds[fieldsTypes]
        val typeRepr = TypeRepr.of[T]
        val typeFields = typeRepr.typeSymbol.caseFields.map(_.name)
        val initial = '{ Ordering.Equal }

        val cmpBody: (Expr[T], Expr[T]) => Expr[Ordering] = 
          (lhse, rhse) => {
            def go(ords: List[Expr[Ord[_]]], fieldNames: List[String]): Expr[Ordering] =
              import quotes.reflect._
              ords match
                case h :: t =>
                  val orda = '{ $h.asInstanceOf[Ord[Any]] }
                  val fieldGetter = fieldNames.head
                  val lhss = Select.unique(lhse.asTerm, fieldGetter).asExpr
                  val rhss = Select.unique(rhse.asTerm, fieldGetter).asExpr
                  '{
                     val cmpResult = $orda.compare($lhss, $rhss)
                     if cmpResult != Ordering.Equal then cmpResult
                     else ${go(t, fieldNames.tail)} }
                case _ => 
                  '{ Ordering.Equal }

            go(fieldOrds, typeFields)      
          }
        '{
          new Ord[T]:
            def compare(lhs: T, rhs: T): Ordering = ${cmpBody('lhs,'rhs)}
        }
      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = childrenTypes} } => 
        val cmpBody: (Expr[Seq[Ord[?]]], Expr[T], Expr[T]) => Expr[Ordering] = 
          (ords, lhse, rhse) => {
            val lOrdinal = '{ $m.ordinal($lhse) }
            val rOrdinal = '{ $m.ordinal($rhse) } 
            '{
              if $lOrdinal < $rOrdinal then Ordering.LessThan
              else if $lOrdinal > $rOrdinal then Ordering.GreaterThan
              else $ords($lOrdinal).asInstanceOf[Ord[T]].compare($lhse, $rhse)
            }
          }

        '{
          new Ord[T]:
            private val ords = ${ Expr.ofSeq(summonOrDeriveOrds[childrenTypes]) }
            def compare(lhs: T, rhs: T): Ordering = ${cmpBody('ords, 'lhs, 'rhs)}
        }  

  private def extractFieldsE[T: Type](using Quotes) =
    Type.of[T] match
      case '[EmptyTuple] => Nil
      case '[l *: ts] => 
      case other => ???  

  private def summonOrDeriveOrds[T: Type](using Quotes): IndexedSeq[Expr[Ord[_]]] =
    Type.of[T] match
      case '[EmptyTuple] => Vector.empty
      case '[t *: ts] => summonOrDeriveOrd[t] +: summonOrDeriveOrds[ts]   

  private def summonOrDeriveOrd[T: Type](using Quotes): Expr[Ord[T]] =
    Expr.summon[Ord[T]].getOrElse(derived[T])

  private def summonOrds[T: Type](using Quotes): List[Expr[Ord[_]]] =
    Type.of[T] match
      case '[EmptyTuple] => Nil
      case '[t *: ts] => summonOrd[t] :: summonOrds[ts]

  private def summonOrd[T: Type](using Quotes): Expr[Ord[_]] =
    Expr.summon[Ord[T]]
      .getOrElse(
        quotes.reflect.report.throwError(s"Can't find Encoder for type: ${Type.show[T]}")
      ) 