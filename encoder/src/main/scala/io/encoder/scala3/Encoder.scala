package io.encoder.scala3

import scala.CanEqual.derived
import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.annotation.tailrec

enum Json:
    case JsonNull
    case JsonBoolean(value: Boolean)
    case JsonInt(value: Long)
    case JsonFloat(value: Double)
    case JsonString(value: String)
    case JsonValues(value: Seq[Json])
    case JsonObject(value: Map[String, Json])

    def prettyString(): String = 
      this match
        case Json.JsonNull => "null"
        case Json.JsonBoolean(value) => value.toString()
        case Json.JsonInt(value) => value.toString()
        case Json.JsonFloat(value) => value.toString()
        case Json.JsonString(value) => s"\"$value\""
        case Json.JsonValues(values) => values.map(_.prettyString()).mkString("[", ",", "]")
        case Json.JsonObject(value) => value.map(e => s"\"${e._1}\": ${e._2.prettyString()}").mkString("{\n  ", ",\n  ", "\n}")

// Type class definition
trait Encoder[@specialized(Int, Boolean) A]:
  def encode(a: A): Json
  
  // Extension block defines additional, convenient methods for callers to be used.
  extension(a: A)
    def asJson(): Json = encode(a)

object Encoder:    
  def apply[A: Encoder]: Encoder[A] = summon

  given strEncoder: Encoder[String] with
    def encode(s: String): Json = Json.JsonString(s)

  given Encoder[Boolean] with
    def encode(b: Boolean): Json = Json.JsonBoolean(b)

  given Encoder[Int] with 
    def encode(i: Int): Json = Json.JsonInt(i)

  given seqEncoder[A](using A: Encoder[A]): Encoder[Seq[A]] with
    def encode(s: Seq[A]): Json = Json.JsonValues(s.map(_.asJson()))

  given optionEncoder[A](using A: Encoder[A]): Encoder[Option[A]] with
    def encode(oa: Option[A]): Json = oa.fold(Json.JsonNull)(_.asJson())

  inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = 
    inline m match
      case p: Mirror.ProductOf[T] => 
        MacroBased.deriveProductEncoder[T](p)
      case s: Mirror.SumOf[T] => 
        MacroBased.deriveSumEncoder[T](s)


  trait Approach: 
    inline def deriveProductEncoder[T](p: Mirror.ProductOf[T]): Encoder[T]
    inline def deriveSumEncoder[T](s: Mirror.SumOf[T]): Encoder[T]

  /*
  * ==================        
  * Deriving Encoders- naive appraoch
  * ==================
  * Mean: ~290
  * 99p: ~540
  */
  object ShallowMirror extends Approach:     
    inline def deriveProductEncoder[T](p: Mirror.ProductOf[T]): Encoder[T] =
      deriveProductEncoder[T](p, false)

    private inline def deriveProductEncoder[T](p: Mirror.ProductOf[T], inline withDiscriminator: Boolean): Encoder[T] = //Slow... ~250ns to transform to JSON.
      val fieldEncoders = summonEncoders[p.MirroredElemTypes]
      new Encoder[T]:
        def encode(t: T): Json =
          val tp = t.asInstanceOf[scala.Product]
          val fields = tp.productElementNames
              .zip(tp.productIterator)
              .zip(fieldEncoders)
              .map { case ((name, value), encoder) =>
                name -> encoder.asInstanceOf[Encoder[Any]].encode(value)
              }
              .toMap
          Json.JsonObject(
            inline if withDiscriminator then 
              fields + (("type", Encoder[String].encode(extractString[p.MirroredLabel])))
            else fields
          )

    private inline def extractString[T]: String =
      inline constValue[T] match
        case str: String => str          

    private inline def summonEncoders[FieldTypes <: Tuple]: List[Encoder[_]] =
      inline erasedValue[FieldTypes] match
        case _: EmptyTuple => Nil
        case _: (t *: ts) => summonInline[Encoder[t]] :: summonEncoders[ts]                

    inline def deriveSumEncoder[T](s: Mirror.SumOf[T]): Encoder[T] =
      new Encoder[T] {
        private val encoders = deriveSumCasesEncoders[s.MirroredElemTypes]  

        def encode(t: T): Json =  
          encoders.apply(s.ordinal(t)).asInstanceOf[Encoder[Any]].encode(t)
      }

    private inline def deriveSumCasesEncoders[CaseTypes <: Tuple]: IndexedSeq[Encoder[_]] =
      inline erasedValue[CaseTypes] match
        case _: EmptyTuple => IndexedSeq.empty
        case _: (t *: ts)  => deriveSumProductEncoder[t] +: deriveSumCasesEncoders[ts] 

    private inline def deriveSumProductEncoder[T]: Encoder[T] =
      summonFrom { 
        case p: Mirror.ProductOf[T] => deriveProductEncoder[T](p, true)
      }

  /*
  * =========================================
  * Deriving encoders - intermediate approach
  * =========================================
  * Mean: ~40
  * 99p: ~250
  */    
  object ExhaustingMirror extends Approach:
    inline def deriveProductEncoder[T](p: Mirror.ProductOf[T]): Encoder[T] = 
      deriveProductEncoder(p, false)

    private inline def deriveProductEncoder[T](p: Mirror.ProductOf[T], inline withDiscriminator: Boolean): Encoder[T] = 
      new Encoder[T]:
        def encode(t: T): Json = 
          val acc = 
            inline if withDiscriminator then 
              Map.empty[String, Json] + (("type", Encoder[String].encode(extractString[p.MirroredLabel])))
            else Map.empty[String, Json]
          buildRecursively[p.MirroredElemLabels, p.MirroredElemTypes](
            acc = acc,
            index = 0,
            instance = t.asInstanceOf[scala.Product]
          ) 

    private inline def extractString[T]: String = 
      inline constValue[T] match
        case str: String => str

    private inline def buildRecursively[Labels <: Tuple, FieldTypes <: Tuple](acc: Map[String, Json], index: Int, instance: scala.Product): Json.JsonObject = // Much faster! Over 42 ns.
      inline erasedValue[(Labels, FieldTypes)] match
        case _: ((l *: ls), (t *: ts)) => 
          val fieldName = extractString[l]
          val fieldEncoder = summonInline[Encoder[t]]
          val fieldValue = instance.productElement(index).asInstanceOf[t]
          val pair = (fieldName, fieldEncoder.encode(fieldValue))
          buildRecursively[ls, ts](acc + pair, index + 1, instance)
        case _ => Json.JsonObject(acc)

    inline def deriveSumEncoder[T](s: Mirror.SumOf[T]): Encoder[T] = 
      new Encoder[T]:
        private val casesEncoder = {
          val encodersArray = Array.ofDim[Encoder[T]](extractSize[s.MirroredElemLabels])
          deriveSumCasesEncoders[T, s.MirroredElemTypes](encodersArray, 0)
        }

        def encode(t: T): Json = 
          casesEncoder.apply(s.ordinal(t)).encode(t)

    private inline def extractSize[T <: Tuple] =
      constValueTuple[T].size
      
    private inline def deriveSumCasesEncoders[T, CaseTypes <: Tuple](acc: Array[Encoder[T]], index: Int): Array[Encoder[T]] = 
      inline erasedValue[CaseTypes] match
        case _: EmptyTuple => acc
        case _: (t *: ts) => 
          acc(index) = deriveSumProductEncoder[t].asInstanceOf[Encoder[T]]
          deriveSumCasesEncoders[T, ts](acc, index + 1)
          

    private inline def deriveSumProductEncoder[T]: Encoder[T] =
      summonFrom { 
        case p: Mirror.ProductOf[T] => deriveProductEncoder(p, true)
      }

  /*
  * =========================================
  * Deriving encoders - with Macros element.
  * =========================================
  */    
  object MacroBased extends Approach:  
    inline def deriveProductEncoder[T](p: Mirror.ProductOf[T]): Encoder[T] = 
      derivedProductEncoder(p, false)
    inline def deriveSumEncoder[T](s: Mirror.SumOf[T]): Encoder[T] = 
      new Encoder[T]:
        private val casesEncoder = {
            val encodersArray = Array.ofDim[Encoder[T]](extractSize[s.MirroredElemLabels])
            deriveSumProductsEncoders[s.MirroredElemTypes, T](encodersArray, 0)
          }

        def encode(t: T): Json = 
          casesEncoder.apply(s.ordinal(t)).encode(t)

    private inline def derivedProductEncoder[T](p: Mirror.ProductOf[T], inline withDiscriminator: Boolean): Encoder[T] =
      new Encoder[T]:
        def encode(t: T): Json =
          val initialMap = 
            inline if withDiscriminator then
              val typeJsoned = summonInline[Encoder[String]].encode(constValue[p.MirroredLabel])
              Map.empty[String, Json] + (("type", typeJsoned))
            else Map.empty[String, Json]
          go[p.MirroredElemLabels, p.MirroredElemTypes, T](initialMap, t)
  
    private inline def go[FieldsLabels <: Tuple, FieldsTypes <: Tuple, I](acc: Map[String, Json], instance: I): Json.JsonObject =
      inline erasedValue[(FieldsLabels, FieldsTypes)] match
        case _: (l *: ls, t *: ts) =>        
          go[ls, ts, I](acc + makeJsonEntry[l, t, I](instance), instance)      
        case _ => Json.JsonObject(acc)
  
    private inline def makeJsonEntry[FieldLabel, FieldType, I](instance: I): (String, Json) = 
      ${ makeJsonEntryMacro[FieldLabel, FieldType, I]('instance) }
  
    private final def makeJsonEntryMacro[FieldLabel: Type, FieldType: Type, I](instance: Expr[I])(using Quotes): Expr[(String, Json)] = 
      import quotes.reflect.*
      val fieldName = Type.valueOfConstant[FieldLabel].getOrElse(report.throwError("Non-const value met!")).asInstanceOf[String]
      val encoder = Expr.summon[Encoder[FieldType]].getOrElse(report.throwError(s"Missing encoder for: $fieldName"))
      '{
        val fieldNameV = ${Expr(fieldName)}
        val selected = ${Select.unique(instance.asTerm, fieldName).asExprOf[FieldType]}
        val json = $encoder.encode(selected)
        (fieldNameV, json)
      }
  
    private inline def extractSize[Labels <: Tuple]: Int = 
      constValueTuple[Labels].size

    private inline def deriveSumProductsEncoders[CasesTypes <: Tuple, T](acc: Array[Encoder[T]], index: Int): Array[Encoder[T]] =
      inline erasedValue[CasesTypes] match
        case _: EmptyTuple => acc
        case _: (c *: cs) => 
          acc(index) = deriveSumProductEncoder[c].asInstanceOf[Encoder[T]]
          deriveSumProductsEncoders[cs, T](acc, index + 1)
  
    // Must be defined separately    
    private inline def deriveSumProductEncoder[T] = 
      summonFrom { 
        case p : Mirror.ProductOf[T] => derivedProductEncoder(p, true)
      }    