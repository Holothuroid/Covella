/**
  * Created by 1of3 on 31.10.2016.
  */


package org.covella.dates

import org.covella.dsl.{Calendar, InferedUnit, TimeUnit}


/**
  * This trait enriches types that represents dates with various outputs according to a Calendar object.
  * A calendar is looked for in implicit scope.
  * Known implementors: Datum, TimeStamp, ZeroDatum
  */

trait DateLike{
  def get(unit: TimeUnit,context: TimeUnit = InferedUnit)(implicit cal: Calendar) : Option[BigInt]

  def format(formatKey: String)(implicit cal: Calendar) = cal.dateFormats(formatKey).makeString(this)

  def getName(unit: TimeUnit)(implicit cal: Calendar): String = ???
  def getAlias(unit: TimeUnit, aliasKey : String)(implicit cal: Calendar) = ???

}








/**
  * ZeroDatum represents a Datum, where all units are at their offset (either 0 or 1)
  * This datum might have contradictions in a particular calender.
  */

object ZeroDatum extends DateLike with Calendable {
  override def get(unit: TimeUnit, context: TimeUnit = InferedUnit)(implicit cal: Calendar) = if (unit.countingFromZero) Some(0) else Some(1)
  override def toString = "ZeroDatum"
  def timestamp(implicit cal: Calendar) = ???
}


/**
  * Marker trait used in Calendar for timeStampZero
  */

private[covella] trait Calendable extends DateLike{
  def timestamp(implicit cal: Calendar) : Option[Timestamp]

}