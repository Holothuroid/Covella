package com.github.holothuroid.covella

import scala.language.implicitConversions


/**
  *  An Era is the top level element in a Calendar. It works similar to Measurables, except that it is infinite.
  *  An Era goes forward and backwards in time; it is 'proleptic' as the calendar geeks call it.
  *  Even though names for years are common in many cultures, the current implementation for Era does not include fields for such names.
  *  The reason is that the Era class is mainly concerned with issues of intercalation, and year names, regal eras, etc.
  *  often run independently of intercalation.
  *  If you need those, I suggest a custom DateFormatHelper.
  *
  */

trait Era extends DateHandler with  PartialFunction[BigInt,Option[TimeUnit]] {

  /**
    * This method tries to convert the Era to a simpler form: Mixed -> Periodic -> Homogenous.
    * Automatically called by Calendar.apply.
    * @return An Era, either a simpler form or the starting object itself.
    */

  def optimise: Era
  def unitDesignation: Symbol

  def excludingShiftPrevious(is: BigInt*) = ExcludingShiftPrevious(this,is.toSet)
  def excludingShiftLater(is: BigInt*) = ExcludingShiftLater(this,is.toSet)
}



abstract class ConcreteEra extends Era  {

  def ticksOf(i : BigInt) : BigInt
  def ticksUntil (i: BigInt) : BigInt

  def timestamp(datum: Datum): Option[BigInt] =
    for(index <- datum.get(unitDesignation) if isDefinedAt(index);
        maybeSubsequentTicks <- apply(index).map(_.timestamp(datum));
        subsequentTicks <- maybeSubsequentTicks )
      yield ticksUntil(index)  + subsequentTicks

  def timestampOrZero(datum: Datum): BigInt =
    ticksUntil(datum.get(unitDesignation).getOrElse(0)) +
      ( for(index <- datum.get(unitDesignation) if isDefinedAt(index);
            subsequentTicks <- apply(index).map(_.timestampOrZero(datum)) )
        yield  subsequentTicks ).getOrElse(0)


  def check(datum: Datum) : Datum = {
    val newDatum = datum.update(unitDesignation,_.requireAllowed(!isDefinedAt(_)) )

    newDatum.get(unitDesignation) match {
      case None => newDatum.remove(subunits.tail)
      case Some(i) => apply(i).get.check(newDatum)
    }
  }


}

object Era {
  def apply(measurable: TimeUnit) = HomogeneousEra(measurable)
  implicit def measurable2era(measurable: TimeUnit) : HomogeneousEra = HomogeneousEra(measurable)

  def given(condition_ : BigInt=>Boolean) =
    new { def have(value: TimeUnit) = MixedEra(Seq(EraClause(condition_ ,value))) }

  def given( is : BigInt *) =
    new { def have(value: TimeUnit) = MixedEra(Seq(EraClause(is.contains(_) ,value))) }
}



/**
  * A simple era without leap years / intercalation.
  * @param unit The only top level element the calendar has.
  */


case class HomogeneousEra(unit: TimeUnit) extends ConcreteEra {
  def subunits = unit.subunits
  def isDefinedAt(i: BigInt): Boolean = true
  def ticksOf(i: BigInt): BigInt = unit.ticks
  def unitDesignation: Symbol = unit.designation
  def apply(i: BigInt): Option[TimeUnit] = Some(unit)
  def optimise = this

  def byTicks(tocks: BigInt): Either[String, Datum] = {
    val within : BigInt = tocks / unit.ticks
    val index = if (tocks >= 0 || (tocks-unit.ticks*within).abs==0) within else within -1
    val remainder : BigInt = (tocks - index*unit.ticks).abs
    unit.byTicks(remainder) map { _ & new Datum(unitDesignation -> Ok(index)) }
  }

  def ticksUntil(i: BigInt): BigInt = i * unit.ticks

  override def timestamp(datum: Datum): Option[BigInt] =
    (for(i <- unit.timestamp(datum)) yield datum.get(unitDesignation).map(_ * unit.ticks +i)).flatten

  override def timestampOrZero(datum: Datum): BigInt = datum.getOrElse(unitDesignation,0)  * unit.ticks + unit.timestampOrZero(datum)

  override def toString(): String = s"Era($unit)"
}





/**
  * An era with leap years or similar.
  * @param clauses This sequence of EraClause forms a partial function to handle leap years and similar.
  *                On application, the era will deliver the Measurable from the first matching clause.
  */

case class MixedEra(clauses: Seq[EraClause] /*Refined NonEmpty*/) extends ConcreteEra   {
  val subunits = clauses.head.value.subunits
  val unitDesignation  = subunits.head

  require( clauses.forall(_.value.subunits == subunits) )

  def apply(i: BigInt) : Option[TimeUnit]  = clauses.withFilter(_.condition(i)).map(_.value).headOption
  def isDefinedAt(i: BigInt): Boolean = apply(i).nonEmpty
  def ticksOf(i : BigInt) : BigInt = apply(i).map(_.ticks).getOrElse(0)

  def ticksUntil (i: BigInt) : BigInt =
    if (i>=0) countUp(i,0,0)._2
    else      countDown(i,0,0)._2

  def optimise =
    if (clauses.map(_.value).toSet.size == 1) HomogeneousEra(clauses.head.value)
    else if ( clauses.map(_.condition).forall(_.isInstanceOf[PeriodicFunction] ) ) PeriodicEra.fromClauses(clauses)
    else this

  def byTicks(tocks: BigInt) : Either[String,Datum] = {

    val (maybeIndex,remainder) : (BigInt,BigInt) = tocks match{
      case x if x == 0 => (0,0)

      case x if x > 0 =>  val quartuple = findPositive(tocks,0,0,0,0)
                          (quartuple._3, tocks-quartuple._4)

      case x if x < 0 => val quartuple = findNegative(tocks,0,0,0,0)
     //   println(quartuple)
         if ((quartuple._4-tocks).abs == 0)  (quartuple._3, 0)
         else
           (quartuple._1,(quartuple._2-tocks+1).abs)
    }

    val index = findApplicable(maybeIndex)

    apply(index).get.byTicks(remainder) map {_ & new Datum(unitDesignation -> Ok(index))}
  }


  def findApplicable(i: BigInt) : BigInt = i match {
    case x if isDefinedAt(x) => x
    case x if x>=0 => findApplicable(x+1)
    case x if x<0 => findApplicable(x-1)

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
    new { def have(value: TimeUnit) = MixedEra(clauses :+  EraClause(condition_ ,value)) }

  def default(value: TimeUnit) = MixedEra(clauses :+ EraClause(Pass,value))
}


case class EraClause(condition: BigInt => Boolean, value: TimeUnit)


