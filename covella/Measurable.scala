package com.githup.holothuroid.covella

/**
  * Created by 1of3 on 09.06.2017.
  */

/**
  * A TimeUnit with a fixed length (_.ticks).
  */

trait Measurable extends TimeUnit with DateHandler {
  def ticks: BigInt

}


/**
  * The basic unit in a Calendar with exactly one tick.
  * @param designation The name for this unit as a Symbol.
  */


case class Tick(designation: Symbol) extends DateHandler with Measurable
{val ticks = 1
  val subunits = List(designation)

  def check(seq: Seq[String]) = if(seq.isEmpty) new Datum() else new Datum('overflow ->Unknown(seq.mkString("-")))
  def byTicks(tocks: BigInt) = if (tocks==0) Right(Datum()) else Left("I'm a tick! You gave me:"+tocks)
  def timestamp(datum: Datum) : Option[BigInt] = if (datum.isOkAt(designation)) Some(0) else None
}


/**
  * The second simplest type of TimeUnit: It repeats another Measurable a certain numer of times.
  * @param designation The name for this unit as a Symbol.
  * @param unit The repeated Measurable.
  * @param amount How often?
  */


case class Measure(designation: Symbol,unit: Measurable, amount: BigInt /*Refined Positive*/) extends DateHandler with Measurable {
  require(! unit.subunits.contains(designation) )
  lazy val ticks = amount * unit.ticks
  lazy val subunits = designation :: unit.subunits

  def check(seq: Seq[String]) : Datum =
    if (seq.isEmpty)  Datum()
    else new Datum(unit.designation->headEntry(seq,upper=amount,lower=0)) & (if(seq.length>1) unit.check(seq.tail) else Datum())


  def byTicks(tocks: BigInt) : Either[String,Datum] = {
    tocks match { case x if x >= 0 && x < ticks => unit.byTicks(x% unit.ticks) map (_ & new Datum(unit.designation->Ok(x/unit.ticks)) )                                                                   // unit.byTicks(x% unit.ticks) )
    case _ => Left(s"Something went wrong in Measure $designation. Got: $tocks Have: $ticks") }

  }

  def timestamp(datum: Datum) =
    for(index <- datum get unit.designation  ;
        subsequentTicks <- unit.timestamp(datum))  //;
      yield unit.ticks *index + subsequentTicks

}


/**
  * This class can be used on subequent calendars in CalendarSystem to expose certain units that would be shadowed by
  * @param designation
  * @param unit
  */

case class TimeUnitAlias(designation: Symbol, unit: Measurable) extends Measurable{
  def ticks: BigInt = unit.ticks
  def subunits: List[Symbol] = designation :: unit.subunits.tail
  def check(seq: Seq[String]): Datum = unit.check(seq)
  def byTicks(tocks: BigInt): Either[String, Datum] = unit.byTicks(tocks)

  def timestamp(datum: Datum): Option[BigInt] = {
    val newEntries = datum.entries.map { case (designation,value) => (unit.designation,value)
                                         case foo => foo}

    unit.timestamp(datum.copy(entries = newEntries))
  }
}