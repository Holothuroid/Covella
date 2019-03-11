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

  type Out <: Calendar
  def additionalAccessors : Map[Symbol,Datum=>DatumEntry]
  protected def addEntry(keyAndFunction: (Symbol, Datum => DatumEntry)): Out


  def addIndex(keyAndFunction : (Symbol, Datum => Int)) : Out =
    addEntry(keyAndFunction._1, x => Extra(Option(keyAndFunction._2(x)),None))

  def addName(keyAndFunction : (Symbol, Datum => String)) : Out =
    addEntry(keyAndFunction._1, x => Extra(None,Option(keyAndFunction._2(x))))

  def add(keyAndFunction : (Symbol, Datum => (Int,String))) : Out =
    addEntry(keyAndFunction._1, x =>
      Extra(Option(keyAndFunction._2(x)._1),Option(keyAndFunction._2(x)._2)))

  def addAdditionals(datum: Datum): Datum = {
    val entries = for ((key, function) <- additionalAccessors) yield (key,function(datum))
    new Datum(entries,Some(this)) & datum
  }

  /**
    * Create a datum from pairing the given numbers with this Calendar's primary units.
    * @param ints Indices for this calendars primary units.
    * @return A Datum in this calendar.
    */

  def datum (ints: BigInt*) =
    Datum.of(primaryUnits.zip(ints) :_*) completeWithCalendar this
}


object Calendar{
  def apply(era: Era,
            tsz: BigInt = BigInt(0),
            acc: Map[Symbol,Datum=>DatumEntry] = Map()) = SimpleCalendar(era.optimise,tsz,acc)
}


/**
  * The most basic Calendar.
  * @param era A hierarchy of TimeUnits wrapped in an Era.
  * @param timestampZero The offset to determine which Datum corresponds to Timestamp(0) in this Calendar.
  *                      For convenience try `.setTimestampZero`
  */

case class SimpleCalendar(era: Era,
                          timestampZero: BigInt = BigInt(0),
                          additionalAccessors : Map[Symbol,Datum=>DatumEntry] = Map())
  extends Calendar {

  type Out = SimpleCalendar
  def primaryUnits : Seq[Symbol] = era.subunits
  def units = primaryUnits.toSet union additionalAccessors.keySet
  def synchronise(that: Calendar) =  that match {
    case SimpleCalendar(e,i,a) => CalendarSystem(Vector(this,that))
    case CalendarSystem(seq,acc) => CalendarSystem(Vector(this)++seq,acc)
  }

  private[covella] def interpret(timestamp: Timestamp) : Datum  =
    addAdditionals(era.byTicks(timestamp.value + timestampZero).toOption.get.copy(cal = Some(this)))

  def timestamp(datum: Datum): Option[Timestamp] = era.timestamp(datum).map(_ - timestampZero).map(Timestamp(_))

  def timestampOrZero(datum: Datum): Option[Timestamp] = {
    if (datum.isOkAt(era.unitDesignation)) Some(Timestamp(era.timestampOrZero(datum)- timestampZero))
    else None
  }

  /**
    * Create a new calendar with `.timestampZero` fitting the given Datum.
    * @param datum A Datum
    * @return A new SimpleCalendar.
    */

  def setTimestampZero(datum: Datum ): SimpleCalendar = {
     check(datum).begins match {
       case Some(timestamp) => copy(timestampZero = timestamp.value)
       case None => throw new IllegalArgumentException("Datum has no fixed beginning. Unable to set Timestamp 0.")
     }
  }


  /**
    * Pair the given numbers with this Calendar's primary units,
    * then call `.setTimestampZero` with the resulting Datum.
    * @param ints A list of numbers corresponding to incdices of this calendars primary units.
  * @return A new SimpleCalendar.
    */

  def setTimestampZero(ints: BigInt*) : SimpleCalendar =  setTimestampZero ( datum(ints :_*) )





  def check(datum: Datum): Datum = addAdditionals(era.check(datum.copy(cal = Some(this))))

  def parse(string: String)(implicit df: DateFormat) : Datum = check(df.parse(string))

  override def toString = s"Calendar(${primaryUnits.mkString(",")};$timestampZero)"

  override def addEntry(keyAndFunction: (Symbol, Datum => DatumEntry)) : Out =
    this.copy(additionalAccessors = additionalAccessors.updated(keyAndFunction._1,keyAndFunction._2))

}


case class CalendarSystem(cals: Seq[Calendar],
                          additionalAccessors : Map[Symbol,Datum=>DatumEntry] = Map()) extends Calendar{

  type Out = CalendarSystem
  lazy val primaryUnits  = cals.head.primaryUnits
  lazy val units : Set[Symbol] = cals.map(_.units).reduce(_ union _ ) union additionalAccessors.keySet

  def synchronise(that: Calendar) =  that match {
    case CalendarSystem(seq,acc) => CalendarSystem(cals ++seq,additionalAccessors ++ acc)
    case _ => CalendarSystem(cals ++ Seq(that),additionalAccessors)

  }

  def parse(string : String)(implicit df: DateFormat) : Datum =  cals.map(_.parse(string)(df)).reduce(_&_)  // todo: Test shadowing!

  def interpret(timestamp: Timestamp) : Datum =
    addAdditionals(cals.map(_.interpret(timestamp)).reduce(_&_).copy(cal = Some(this)))

  def timestamp(datum: Datum): Option[Timestamp] = cals.head.timestamp(datum) // todo: Timestamping should work if any one calendar can stamp and there are no contradictions.
  def timestampOrZero(datum: Datum): Option[Timestamp] = {
    cals.head.timestampOrZero(datum)
  } // todo: see above.

  override def toString: String = s"CalendarSystem(${cals.mkString(", ")};$units)"

  override def check(datum: Datum): Datum =
    addAdditionals(cals.map(_.check(datum)).reduce(_ & _))

  override def addEntry(keyAndFunction: (Symbol, Datum => DatumEntry)) : Out =
    this.copy(additionalAccessors = additionalAccessors.updated(keyAndFunction._1,keyAndFunction._2))
}



