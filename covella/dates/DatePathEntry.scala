package org.covella.dates

import org.covella.dsl.TimeUnit

/**
  * Created by 1of3 on 20.02.2017.
  */

case class DatePathEntry(unit: TimeUnit, index: BigInt, marker: DatePathMarker)


sealed trait DatePathMarker

/**
  * Marks DatePathEntries with an index too high for implied TimeUnitInstance.
  * @param tooHighBy The excessive amount.
  */
case class TooHigh(tooHighBy: Int) extends DatePathMarker{  override def toString = s"[TooHigh $tooHighBy]" }

/**
  * Marks DatePathEntries with an index too low for implied TimeUnitInstance.
  * @param tooLowBy The excessive amount.
  */
case class TooLow(tooLowBy: Int) extends DatePathMarker{   override def toString = s"[TooLow $tooLowBy]" }
/**
  * Marks DatePathEntries without flaw.
  */

case object OK extends DatePathMarker{   override def toString = "" }

/**
  * Marks DatePathEntries with an index that is excluded from the implied TimeUnitInstance.
  * Cf. TimeUnitInstance#excluding
  */
case object Forbidden extends DatePathMarker{   override def toString = "[Forbidden]" }
/**
  * Marks DatePathEntries with a TimeUnit repeated in the same DatePath.
  * Can happen when Timestamp#interpret is called with an explicit date path.
  */
case object Recursive extends DatePathMarker{   override def toString = "[Circular]" }

/**
  * Marks a DatePathEntry with a unknown name associated for the implied TimeUnitInstance.
  */
case object Unknown extends DatePathMarker{   override def toString = "[Unknown]" }

/**
  * Marks a DatePathEntry, when the unit does not correspond to the given index.
  * Can happen when Parenting unit has different kinds of children.
  * Cf. Parenting#counts and Parenting#subunits.
  */

case object Unavailable extends DatePathMarker{   override def toString = "[Unavailable]" }

/**
  * Marks DatePathEntries with with a TimeUnit  not found in the Calendar.
  * Can happen when Timestamp#interpret is called with an explicit date path.
  */
case object NonExistent extends DatePathMarker{   override def toString = "[NonExistentUnit]" }
