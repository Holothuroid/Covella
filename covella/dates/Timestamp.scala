package org.covella.dates

import org.covella.Preamble.DatePath
import org.covella._
import org.covella.dsl._

/**
  * This value class wraps timestamp for use with a Calendar.
  * It is an efficient means to store a date and purely optional.
  * @param value A probably very Long number.
  */

case class Timestamp(value: BigInt) extends DateLike {

  /**
    * This method creates a CalendricDate with a single path based on the calendar's primary and secondary
    * cycles.
    */


  def interpret(implicit cal: Calendar) : CalendricDate = {
    var result : DatePath = Seq()
    val initial = this.value + cal.timeStampZero.timestamp(cal).map(_.value).getOrElse(0)

    def nextEntry(parent: Parenting, tocks: BigInt) : Unit = {
      val cache =  parent.byTicks(tocks)
      result :+= DatePathEntry( cache._1.unit, cache._1.index, OK)
      if (cache._2 > 1) nextEntry(cache._1.asInstanceOf[Parenting],cache._2) }

    nextEntry(cal.era,initial)

    PointDate(Set(result),cal) }



  /**
    * This method creates a CalendricDate for a given time unit path and calendar. The
    * counting is strict.
    */

  def interpret(tus: TimeUnit*)(implicit cal: Calendar) : CalendricDate = ???


  def datum(implicit cal: Calendar) = interpret(cal).datum



  def get(unit: TimeUnit, context: TimeUnit = InferedUnit)(implicit cal: Calendar) =  this.interpret(cal).get(unit,context)(cal)

  def + (that: Timestamp) : Timestamp = Timestamp(this.value + that.value)
  def plus (that: Timestamp): Timestamp  = this + that
  def - (that: Timestamp): Timestamp  = Timestamp(this.value - that.value)
  def minus (that: Timestamp): Timestamp  = this - that

  def plus(that: Uniform) : Timestamp = Timestamp(this.value + that.ticks)
  def + (that: Uniform) : Timestamp  = this plus that
  def minus(that: Uniform): Timestamp  = Timestamp(this.value - that.ticks)
  def - (that: Uniform): Timestamp  = this minus that
  def plus(that: CycledInstanceBuilder) : Timestamp = Timestamp(this.value + that.childUnit.ticks * that.size)
  def + (that: CycledInstanceBuilder): Timestamp  = this plus that
  def minus(that: CycledInstanceBuilder) : Timestamp = Timestamp(this.value - that.childUnit.ticks * that.size)
  def - (that: CycledInstanceBuilder): Timestamp  = this minus that

  //def forth(byThat : TimeUnit, number: Int) : Timestamp = this.datum.forth(byThat,number).timestamp.get
  //def back(byThat : TimeUnit, number: Int) : Timestamp = this.datum.back(byThat,number).timestamp.get

}