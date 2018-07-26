package com.github.holothuroid.covella


/**
  * A "computer date", consisting of a very long number.
  *
  * @param value Said number.
  */


case class Timestamp(value: BigInt) extends AnyVal with Timelike {

  def inCalendar(implicit cal: Calendar)  = cal.interpret(this)

  def + (measurable: TimeUnit) = Timestamp(this.value + measurable.ticks)
  def - (measurable: TimeUnit) = Timestamp(this.value - measurable.ticks)
  def + (number: BigInt, measurable: TimeUnit) = Timestamp(this.value + number * measurable.ticks)
  def - (number: BigInt, measurable: TimeUnit) = Timestamp(this.value - number * measurable.ticks)

  def compare(that: Timelike): Int = that match {
    case t: Timestamp => this.value compare t.value
    case Bang => 1
    case Bust => -1
  }
  def sinceZeroIn(measurable: TimeUnit) : BigInt = value/measurable.ticks

  /**
    * Creates a Stream of Timestamps going forward in time, starting from this Timestamp.
    * @param unit The step length, e.g. days, weeks, as a TimeUnit.
    * @return A Stream of Timestamps.
    */

  def forwards(unit: TimeUnit) :  Stream[Timestamp] =  {
    def stream(t: Timestamp) : Stream[Timestamp] =  t #:: stream(t+(1,unit))
    stream(this)
  }

  /**
    * Creates a Stream of Timestamps going back in time, starting from this Timestamp.
    * @param unit The step length, e.g. days, weeks, as a TimeUnit.
    * @return A Stream of Timestamps.
    */


  def backwards(unit: TimeUnit) :  Stream[Timestamp] =  {
    def stream(t: Timestamp) : Stream[Timestamp] =  t #:: stream(t-(1,unit))
    stream(this)
  }

  def till(that: Timestamp) : Option[Interval] = if (this <= that) Some(Interval(this,that)) else None
}

object Timestamp{
  implicit def timestamp2Datum(t: Timestamp)(implicit cal: Calendar) : Datum = t.inCalendar(cal)

}


/**
  * The trait Timelike extends Timestamps with values for negative infinity `Bang` and infinity `Bust`
  */

sealed trait Timelike extends Any with Ordered[Timelike]

object Bang extends Timelike {
  override def compare(that: Timelike): Int = if (that == Bang) 0 else -1
}
object Bust extends Timelike {
  override def compare(that: Timelike): Int = if (that == Bust) 0 else 1
}

object Timelike {

  implicit def timelike2Interval(t: Timelike) = Interval(t,t)
}
