package com.githup.holothuroid.covella

import scala.language.implicitConversions


/**
  * Created by 1of3 on 04.06.2017.
  */



/**
  * A more or less human readable representation of dates, consisting of qualified key-value pairs.
  * @param entries Maps Symbols to DatumEntries.
  * @param cal Optionally, a calendar according to which the DatumEntries are chosen.
  */

case class Datum(entries: Map[Symbol,DatumEntry] = Map(),cal: Option[Calendar] = None){
  import entries.{ isDefinedAt, values}

  def apply(symbol: Symbol) : DatumEntry =
    entries.applyOrElse[Symbol,DatumEntry](symbol,_ => Unknown(s"$symbol not found in Datum"))
  def this(tuples : (Symbol,DatumEntry)*)  = this(tuples.toMap)


  /**
    * The method combines two Datums. It takes all the units from this object, as well as its calendar, if present.
    * It adds those units from the second datum, that are not found in the first.
    *
    * @param that The Datum to be added
    * @return A new Datum.
    */
  def & (that: Datum) : Datum =  Datum(that.entries ++ this.entries,this.cal)

  def get(symbol: Symbol) : Option[BigInt] = apply(symbol) match { case Ok(x,_) => Some(x)
                                                                   case _ => None }
  def getOrElse(symbol: Symbol, default: BigInt) : BigInt = get(symbol).getOrElse(default)

  def getName(symbol: Symbol) : Option[String] = apply(symbol) match { case Ok(_,x) => x
                                                                       case _ => None }
  def getNameOrElse(symbol: Symbol, default: String) : String = getName(symbol).getOrElse(default)

  def format(implicit df: DateFormat) : String = df.format(this)

  def withoutError : Boolean = values.forall(_.isInstanceOf[Ok])
  def isOkAt(unit: Symbol) = isDefinedAt(unit) && apply(unit).isInstanceOf[Ok]
  def isOkUntil : Option[Symbol] = cal.flatMap(_.units.takeWhile(this.isOkAt(_)).lastOption)
  def isComplete = isOkUntil.nonEmpty && isOkUntil == cal.map(_.units.last)




  def begins : Option[Timestamp] = cal.flatMap(_.timestampOrZero(this))
  def ends : Option[Timestamp] = ???
  def timestamp : Option[Timestamp] = cal.flatMap(_.timestamp(this))
  def interval : Option[Interval] = for(start <- begins; end <-ends if start<=end) yield Interval(start,end)

}






sealed trait DatumEntry
case class Ok(index: BigInt, name: Option[String] = None) extends DatumEntry
case class TooHigh(index: BigInt, upperBound: BigInt) extends DatumEntry
case class TooLow(index: BigInt, upperBound: BigInt) extends DatumEntry
case class NameNotFound(name: String) extends DatumEntry
case class NonUniqueName(name: String, indices: Set[BigInt]) extends DatumEntry
case class Unknown(stuff: String) extends DatumEntry



/**
  * A "computer date", consisting of a very long number.
  * @param value Said number.
  */


case class Timestamp(value: BigInt) extends AnyVal with Ordered[Timestamp] {

  def inCalendar(implicit cal: Calendar)  = cal.interpret(this)

  def + (measurable: Measurable) = Timestamp(this.value + measurable.ticks)
  def - (measurable: Measurable) = Timestamp(this.value - measurable.ticks)
  def + (number: BigInt, measurable: Measurable) = Timestamp(this.value + number * measurable.ticks)
  def - (number: BigInt, measurable: Measurable) = Timestamp(this.value - number * measurable.ticks)

  def compare(that: Timestamp): Int = this.value compare that.value
  def sinceZeroIn(measurable: Measurable) : BigInt = value/measurable.ticks

  def upTo(that: Timestamp) : Option[Interval] = if (this <= that) Some(Interval(this,that)) else None
}

object Timestamp{
  implicit def timestamp2Datum(t: Timestamp)(implicit cal: Calendar) : Datum = t.inCalendar(cal)
}


/**
  * This class describes an interval of time.
  * @param starts Timestamp when it starts.
  * @param ends Timestamp when it ends.
  */

case class Interval(starts: Timestamp, ends: Timestamp)  {
  require (starts.value <= ends.value)

  def contains(t: Timestamp) = starts <= t && t <= ends
  def contains(i: Interval) = starts <= i.starts && i.ends <= ends
  def overlaps(that: Interval) = (this contains that.starts) || (this contains that.ends)
  def < (t: Timestamp) = ends < t
  def > (t: Timestamp) = t  < starts
  def < (that: Interval) = this.ends < that.starts
  def > (that: Interval) = this.starts > that.ends


  def intersect(that: Interval) : Option[Interval] =
    if (!(this overlaps that)) None
    else if (that.starts<=this.ends) Some(Interval(that.starts,this.ends))
         else   Some(Interval(this.starts,that.ends))

}