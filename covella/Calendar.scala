package com.github.holothuroid.covella

/**
  * Common interface for simple calendars and calendar systems.
  */


trait Calendar{
  def primaryUnits : Seq[Symbol]
  def units : Set[Symbol]
  def timestamp(datum: Datum) : Option[Timestamp]
  def timestampOrZero(datum: Datum) : Option[Timestamp]
  private[covella] def interpret(timestamp: Timestamp) : Datum
  def parse(string: String)(implicit df: DateFormat) : Datum
  def synchronise(cal: Calendar) : CalendarSystem
  def check(datum: Datum) : Datum
}


object Calendar{
  def apply(era: Era, tsz: BigInt = BigInt(0)) = SimpleCalendar(era.optimise,tsz)
}


/**
  * The most basic Calendar.
  * @param era A hierarchy of TimeUnits wrapped in an Era.
  * @param timestampZero The offset to determine which Datum corresponds to Timestamp(0) in this Calendar.
  *                      For convenience try `.setTimestampZero`
  */

case class SimpleCalendar(era: Era,
                          timestampZero: BigInt = BigInt(0)) extends Calendar {

  def primaryUnits : Seq[Symbol] = era.subunits
  def units = primaryUnits.toSet
  def synchronise(that: Calendar) =  that match {
    case SimpleCalendar(e,i) => CalendarSystem(Vector(this,that))
    case CalendarSystem(seq) => CalendarSystem(Vector(this)++seq)
  }

  private[covella] def interpret(timestamp: Timestamp) : Datum  =
    era.byTicks(timestamp.value + timestampZero).toOption.get.copy(cal = Some(this))

  def timestamp(datum: Datum): Option[Timestamp] = era.timestamp(datum).map(_ - timestampZero).map(Timestamp(_))

  def timestampOrZero(datum: Datum): Option[Timestamp] = {
    if (datum.isOkAt(era.unitDesignation)) Some(Timestamp(era.timestampOrZero(datum)- timestampZero))
    else None
  }

  /**
    * Create a new calendar with `.timestampZero` fitting the given date string.
    * @param datum A Datum
    * @return A new SimpleCalendar.
    */

  def setTimestampZero(datum: Datum) = {
     check(datum).begins match {
       case Some(timestamp) => Calendar(era,timestamp.value)
       case None => throw new IllegalArgumentException("Datum has no fixed beginning. Unable to set Timestamp 0.")
     }
  }


  def check(datum: Datum): Datum = era.check(datum.copy(cal = Some(this)))

  def parse(string: String)(implicit df: DateFormat) : Datum = check(df.parse(string))

  override def toString = s"Calendar(${primaryUnits.mkString(",")};$timestampZero)"

}


case class CalendarSystem(cals: Seq[Calendar]) extends Calendar{

  lazy val primaryUnits  = cals.head.primaryUnits
  lazy val units : Set[Symbol] = cals.map(_.units).reduce(_ union _ )

  def synchronise(that: Calendar) =  that match {
    case CalendarSystem(seq) => CalendarSystem(cals ++seq)
    case _ => CalendarSystem(cals ++ Seq(that))

  }

  def parse(string : String)(implicit df: DateFormat) : Datum =  cals.map(_.parse(string)(df)).reduce(_&_)  // todo: Test shadowing!

  def interpret(timestamp: Timestamp) : Datum = cals.map(_.interpret(timestamp)).reduce(_&_).copy(cal = Some(this))

  def timestamp(datum: Datum): Option[Timestamp] = cals.head.timestamp(datum) // todo: Timestamping should work if any one calendar can stamp and there are no contradictions.
  def timestampOrZero(datum: Datum): Option[Timestamp] = cals.head.timestampOrZero(datum) // todo: see above.

  override def toString: String = "CalendarSystem(" + cals.mkString(", ") +")"

  override def check(datum: Datum): Datum = cals.map(_.check(datum)).reduce(_ & _) // todo: Might be better, if system checks congruence.
}



