package com.githup.holothuroid.covella

/**
  * Common interface for simple calendars and calendar systems.
  */


trait Calendar{
  def primaryUnits : Seq[Symbol]
  def units : Set[Symbol]
  def check(string: String)   :Datum
  def checkTokens(tokens: Seq[(Symbol,String)]) : Datum
  def timestamp(datum: Datum) : Option[Timestamp]
  private[covella] def interpret(timestamp: Timestamp) : Datum
  def parse(string: String)(implicit df: DateFormat) : Datum
  def synchronise(cal: Calendar) : CalendarSystem

}


object Calendar{
  def apply(era: Era, tsz: BigInt = BigInt(0)) = SimpleCalendar(era,tsz)
}


/**
  * This calendar has a hierarchy of time units, wrapped in an era. The lowest unit will be a Tick.
  * The calendar always
  * @param era
  * @param timeStampZero
  */

case class SimpleCalendar(era: Era,
                    timeStampZero: BigInt = BigInt(0)) extends Calendar {

  def primaryUnits : Seq[Symbol] = era.subunits
  def units = primaryUnits.toSet
  def synchronise(that: Calendar) =  that match {
    case SimpleCalendar(e,i) => CalendarSystem(Seq(this,that))
    case CalendarSystem(seq) => CalendarSystem(Seq(this)++seq)
  }


  def check(string: String) : Datum = {
    if (string.trim.isEmpty) return Datum()
    val parts = string.split("[-:]").map(_.trim)
    era.check(parts).copy(cal = Some(this)) }


  def byTicks(i: BigInt)  = era.byTicks(i-timeStampZero)
  private[covella] def interpret(timestamp: Timestamp) : Datum  = byTicks(timestamp.value).toOption.get.copy(cal = Some(this))

  def setTimeStampZero(string: String) = this.copy(timeStampZero = check(string).begins.get.value)

  def parse(string: String)(implicit df: DateFormat) : Datum = {
    val entries : Seq[(Symbol,String)] = df.parse(string)
    checkTokens(entries)
  }

  def checkTokens(tokens: Seq[(Symbol, String)]): Datum = {
    val (founds,unknowns)   = tokens.partition(x => primaryUnits.contains(x._1))

    val results : Seq[String] = for(unit <-primaryUnits ) yield founds.find(_._1 == unit).map(_._2).get
    val errors = unknowns.map(x => x._1 -> Unknown(x._2)).toMap

    era.check(results).copy(cal = Some(this)) &  Datum(errors)
  }


  def timestamp(datum: Datum): Option[Timestamp] = era.timestamp(datum).map(Timestamp(_))

  override def toString = s"Calendar(${primaryUnits.mkString(",")};$timeStampZero)"

}


case class CalendarSystem(cals: Seq[Calendar]) extends Calendar{

  lazy val primaryUnits  = cals.head.primaryUnits
  lazy val units : Set[Symbol] = cals.map(_.units).reduce(_ union _ )



  def synchronise(that: Calendar) =  that match {
    case SimpleCalendar(e,i) => CalendarSystem(cals ++ Seq(that))
    case CalendarSystem(seq) => CalendarSystem(cals ++seq)
  }

  def check(string: String) : Datum = cals.head.check(string)
  def parse(string : String)(implicit df: DateFormat) : Datum = {
    val tokens = df.parse(string)
    checkTokens(tokens)
  }

  def checkTokens(tokens: Seq[(Symbol,String)]) : Datum = {
    val dates = for (cal <- cals) yield cal checkTokens tokens.filter(t => cal.units.contains(t._1))
    val unknowns = tokens.filterNot(x => units.contains(x._1)).map { case (unit,string) => unit->Unknown(string)}.toMap
    Datum(unknowns,Some(this)) & dates.reduce(_&_)
  }

  def interpret(timestamp: Timestamp) : Datum = cals.map(_.interpret(timestamp)).reduce(_&_).copy(cal = Some(this))

  def timestamp(datum: Datum): Option[Timestamp] = cals.head.timestamp(datum)

  override def toString: String = "CalendarSystem(" + cals.mkString(", ") +")"
}

/**
  * DateFormats include methods to turn Strings into Datums and back.
  * @param parse Turns a String into a type that a Calendar's method `checkToken` can handle
  * @param format Formats a Datum. This should work irrespective of the presence of a Calendar.
  */

case class DateFormat(parse: String=>Seq[(Symbol,String)], format: Datum=>String)

/**
  * DateFormatHelpers can be used in  `df""` string interpolation provided by DateFormatFactory.
  * See also `num` and `nam` in the package object for examples.
  * @param parse Similar to DateFormat, except for a single unit.
  * @param format Similar to DateFormat.
  */

case class DateFormatHelper(parse: String =>(Symbol,String),format: Datum=>String)


