package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._


case class PlanetaryWeek(days : TimeUnit = CommonDays.days){

  lazy val weeks = 'week of ('weekday isAliasFor days) withNames
    ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") withOffset 1 // Weekdays are counted from 1, not 0.

  lazy val planetaryWeek = Calendar(weeks)

}





case class WesternCalendar(days : TimeUnit = CommonDays.days) extends MonthsAndYears {

  val planetaryWeek = PlanetaryWeek(days).planetaryWeek


  lazy val days31 = month(31)
  lazy val standardFebruary = month(28)
  lazy val days30 = month(30)
  lazy val leapFebruary = month(29)

  // Month October in the year 1582, when Pope Gregor ordered switching to his new calendar system.
  lazy val october1582 = month(21) excludingAll (5 to 14)

  lazy val monthNames = Vector("January", "February", "March", "April", "May",
    "June","July","August","September","October","November","December")

  lazy val leapYear = 'year isCycleOf (
    days31,leapFebruary,days31,days30, // Jan-Feb-Mar-Apr
    days31,days30,days31,days31,  // May-Jun-Jul-Aug
    days30,days31,days30,days31,  // Sep-Oct-Nov-Dec
  ) withNames(monthNames :_*) withOffset 1

  lazy val standardYear =  'year isCycleOf (
    days31,standardFebruary,days31,days30, // Jan-Feb-Mar-Apr
    days31,days30,days31,days31,  // May-Jun-Jul-Aug
    days30,days31,days30,days31,  // Sep-Oct-Nov-Dec
  ) withNames(monthNames :_*) withOffset 1

  lazy val year1582 =   'year isCycleOf (
    days31,standardFebruary,days31,days30, // Jan-Feb-Mar-Apr
    days31,days30,days31,days31,  // May-Jun-Jul-Aug
    days30,october1582,days30,days31,  // Sep-Oct-Nov-Dec
  ) withNames(monthNames :_*) withOffset 1

  lazy val prolepticJulianEra = Era given divisibleBy(4) have leapYear  default standardYear
  lazy val classicJulianEra = prolepticJulianEra excludingShiftPrevious 0  // In classic counting, there is no year 0.

  lazy val prolepticGregorianEra = Era given
    divisibleBy(400) have leapYear given
    divisibleBy(100) have standardYear given // Gregorian leap rule
    divisibleBy(4) have leapYear default  standardYear   // Julian leap rule

  lazy val classicGregorianEra = prolepticGregorianEra excludingShiftPrevious 0  // In classic counting, there is no year 0.

  lazy val classicJulianCalendar = Calendar(classicJulianEra) setTimestampZero Datum.of('year -> 1970) synchronise
    planetaryWeek.setTimestampZero(Datum.of('week -> 0, 'weekday -> 4))   // We don't really care about the number of the week, hence 0. The day was a Thursday, hence 4.

  lazy val prolepticJulianCalendar = Calendar(prolepticJulianEra)setTimestampZero Datum.of('year -> 1970) synchronise
    planetaryWeek.setTimestampZero(Datum.of('week -> 0, 'weekday -> 4))

  lazy val classicGregorianCalendar = Calendar(classicGregorianEra)setTimestampZero Datum.of('year -> 1970) synchronise
    planetaryWeek.setTimestampZero(Datum.of('week -> 0, 'weekday -> 4))   //


  /**
    * The Era that switches between Julian and Gegorian Era in 1582
    * as determined by Pope Gregor
    */

  lazy val westernEra =  Era given 1582 have year1582 given
    (x => (x > 1582) && divisibleBy(100)(x) && notDivisibleBy(400)(x)) have standardYear given
    (x => divisibleBy(4)(x) && (x != 0)) have leapYear given  // Julian leap rule
    (x => x!=0) have standardYear    // There is no year 0!

  lazy val westernCalendar = Calendar(westernEra) setTimestampZero Datum.of('year -> 1970) synchronise
    planetaryWeek.setTimestampZero(Datum.of('week -> 0, 'weekday -> 4))

}