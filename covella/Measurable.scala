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
  def timestampOrZero(datum: Datum) : BigInt = 0
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


  def timestampOrZero(datum: Datum) : BigInt =
    unit.ticks * datum.get(unit.designation).getOrElse(0) + unit.timestampOrZero(datum)


}


/**
  * This class wraps a Measurable and renames its designation.
  * It can be used in subsequent calendars in a CalendarSystem to expose certain units that would be shadowed by previous entries.
  * @param designation The new designation for the contained unit.
  * @param unit The unit whose designation should be changed.
  */

case class TimeUnitAlias(designation: Symbol, unit: Measurable) extends Measurable{
  val unitDesignation = unit.designation

  def ticks: BigInt = unit.ticks
  def subunits: List[Symbol] = designation :: unit.subunits.tail

  private def dealias(datum: Datum) : Datum = {
    val newEntries = datum.entries.map
          { case (`designation`,value) => (unitDesignation,value)
            case foo => foo}
    datum.copy(entries = newEntries)
  }

  private def alias(datum: Datum) : Datum = {
    val newEntries = datum.entries.map
        { case (`unitDesignation`,value) => (designation,value)
          case foo => foo}
    datum.copy(entries = newEntries)
  }

  def check(seq: Seq[String]): Datum = alias( unit.check(seq) )
  def byTicks(tocks: BigInt): Either[String, Datum] =  unit.byTicks(tocks).map(alias(_))

  def timestamp(datum: Datum): Option[BigInt] = unit.timestamp( dealias(datum) )
  def timestampOrZero(datum: Datum) : BigInt = unit.timestampOrZero( dealias(datum) )
}