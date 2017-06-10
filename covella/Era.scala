package com.githup.holothuroid.covella

import scala.language.implicitConversions

/**
  *  An Era is the top level element in a Calendar. It works similar to Measurables, except that it is infinite.
  *  An Era goes forward and backwards in time; it is 'proleptic' as the calendar geeks call it.
  *  Even though names for years are common in many, the current implementation for Era does not include fields for such names.
  *  The reason is that the Era class is mainly concerned with issues of intercalation, and year names, regal eras, etc.
  *  often run independently of intercalation.
  *  If you need those, I suggest a custom DateFormatHelper.
  *
  * @param clauses This sequence of EraClause forms a partial function to handle leap years and similar.
  *                On application, the era will deliver the Measurable from the first matching clause.
  */

case class Era(clauses: Seq[EraClause] /*Refined NonEmpty*/) extends DateHandler with Function1[BigInt,Option[Measurable]] {
  val subunits = clauses.head.value.subunits
  val unitDesignation  = subunits.head

  require( clauses.forall(_.value.subunits == subunits) )

  def apply(i: BigInt) : Option[Measurable]  = clauses.withFilter(_.condition(i)).map(_.value).headOption
  def isDefinedAt(i: BigInt): Boolean = apply(i).nonEmpty
  def ticksOf(i : BigInt) : BigInt = apply(i).map(_.ticks).getOrElse(0)



  def check(seq: Seq[String]) : Datum =
    headThing(seq) map(i => (i,apply(i))) match
    { case Right((i,Some(measurable)))  => new Datum(unitDesignation->Ok(i)) & measurable.check(seq.tail)
    case _  => new Datum(unitDesignation->Unknown(seq.mkString("-"))) }


  def ticksUntil (i: BigInt) : BigInt = if (i>=0) countUp(i,0,0)._2 else countDown(i,0,0)._2

  def timestamp(datum: Datum) : Option[BigInt] =
    for(index <- datum.get(unitDesignation) if isDefinedAt(index);
        maybeSubsequentTicks <- apply(index).map(_.timestamp(datum));
        subsequentTicks <- maybeSubsequentTicks )
      yield ticksUntil(index) + subsequentTicks


  def byTicks(tocks: BigInt) : Either[String,Datum] = {

    val (index,remainder) : (BigInt,BigInt) = tocks match{
      case x if x == 0 => (0,0)
      case x if x > 0 =>  val quartuple = findPositive(tocks,0,0,0,0); (quartuple._3, tocks-quartuple._4)
      case x if x < 0 => val quartuple = findNegative(tocks,0,0,0,0); (quartuple._1,(quartuple._2-tocks).abs)
    }

    apply(index).get.byTicks(remainder) map {_ & new Datum(unitDesignation -> Ok(index))}
  }




  private def countUp(until : BigInt, current: BigInt, sum: BigInt) : (BigInt,BigInt) = if (current==until) (current,sum) else
    countUp(until,current+1,sum+ticksOf(current))

  private def countDown(until : BigInt, current: BigInt, sum: BigInt) : (BigInt,BigInt) = if (current==until) (current,sum) else
    countDown(until,current-1,sum-ticksOf(current))

  private def findPositive(beyond : BigInt, current: BigInt, sum: BigInt, last: BigInt, lastSum: BigInt)
                                                                  : (BigInt,BigInt,BigInt,BigInt) =
    if (sum > beyond) (current,sum,last,lastSum)
    else findPositive(beyond,current+1,sum+ticksOf(current),current,sum)

  private def findNegative(beyond : BigInt, current: BigInt, sum: BigInt, last: BigInt, lastSum: BigInt)
                                                                : (BigInt,BigInt,BigInt,BigInt) =
    if (sum < beyond) (current,sum,last,lastSum)
    else findNegative(beyond,current-1,sum-ticksOf(current),current,sum)



  override def toString = s"Era(${subunits.mkString(",")})"


  def given(condition_ : BigInt=>Boolean) =
    new { def have(value: Measurable) = Era(clauses :+  EraClause(condition_ ,value)) }

  def default(value: Measurable) = Era(clauses :+ EraClause(x=>true,value))
}


object Era {
  implicit def measurable2Era (measurable: Measurable) : Era = Era(Seq(EraClause(_=>true,measurable)))

  def given(condition_ : BigInt=>Boolean) =
    new { def have(value: Measurable) = Era(Seq(EraClause(condition_ ,value))) }

}


case class EraClause(condition: BigInt => Boolean, value: Measurable)














