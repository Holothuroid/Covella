package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._


case class PlanetaryWeek(days : TimeUnit = CommonDays.days){

  lazy val weeks = 'week of ('weekday isAliasFor days) withNames
    ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") withOffset 1 // Weekdays are counted from 1, not 0.

  lazy val planetaryWeek = Calendar(weeks)

}





case class WesternCalendar(days : TimeUnit = CommonDays.days,
                           monthNames: Seq[String] = Vector("January", "February", "March", "April", "May",
                             "June","July","August","September","October","November","December"),
                           override val prefix: String = "") extends MonthsAndYears {

  lazy val days31 = month(31)
  lazy val standardFebruary = month(28)
  lazy val days30 = month(30)
  lazy val leapFebruary = month(29)

  /**
    * Constructs a year with the given month names.
    * @param monthLengths The months as unnamed TimeUnits.
    * @return A time unit designated as 'year or 'prefixYear with the given month names, counting from 1.
    */

  def westernYear(monthLengths: Seq[TimeUnit]) = localYearSymbol isCycleOf (monthLengths: _*) withNames
    (monthNames :_*) withOffset 1

  /**
    * A vector of the months as TimeUnits with lengths determined in the Julian Reform.
    */

  def julianMonthLengths : Vector[TimeUnit] = Vector(
    days31,standardFebruary,days31,days30, // Jan-Feb-Mar-Apr
    days31,days30,days31,days31,  // May-Jun-Jul-Aug
    days30,days31,days30,days31,  // Sep-Oct-Nov-Dec
  )

  /**
    * The Julian common year.
    */
  lazy val standardYear : WithOffset = westernYear(julianMonthLengths)

  /**
    * The Julian leap year, with an additional day in Februray.
    */
  lazy val leapYear : WithOffset = westernYear(julianMonthLengths updated (2,leapFebruary))


  lazy val prolepticJulianEra = Era given divisibleBy(4) have leapYear  default standardYear
  lazy val classicJulianEra = prolepticJulianEra excludingShiftPrevious 0  // In classic counting, there is no year 0.

  lazy val prolepticGregorianEra = Era given
    divisibleBy(400) have leapYear given
    divisibleBy(100) have standardYear given // Gregorian leap rule
    divisibleBy(4) have leapYear default  standardYear   // Julian leap rule

  lazy val classicGregorianEra = prolepticGregorianEra excludingShiftPrevious 0  // In classic counting, there is no year 0.

  // To add weeks to the calenders, the PlanetaryWeek must be synchronized to Unix Epoch.
  // 1970-01-01 Gregorian was a Thursday.
  // setTimestampZero to 0-4.
  // 4 is the fourth day of the week, hence Thursday.
  // 0 is the global counter for the week cycle.
  // We don't care about that, leave it 0.
  lazy val planetaryWeekBeginning1970 = PlanetaryWeek(days).planetaryWeek setTimestampZero (0,4)


  /**
    * The Julian Calendar with a year 0.
    */

  lazy val prolepticJulianCalendar = Calendar(prolepticJulianEra) setTimestampZero (1970,1,13) synchronise
    planetaryWeekBeginning1970

  /**
    * The Julian Calendar without a year 0.
    */

  lazy val classicJulianCalendar = Calendar(classicJulianEra) setTimestampZero (1970,1,13) synchronise // Julian Calendar is currently 13 days ahead
    planetaryWeekBeginning1970


  /**
    * The Gregorian Calendar with a year 0.
    */
  lazy val prolepticGregorianCalendar = Calendar(prolepticGregorianEra) setTimestampZero 1970 synchronise
    planetaryWeekBeginning1970

  /**
    * The Gegorian Calendar without a year 0.
    */
  lazy val classicGregorianCalendar = Calendar(classicGregorianEra)setTimestampZero 1970 synchronise
    planetaryWeekBeginning1970   //


  /**
    * Constructs a Calendar starting with the Julian Era, then, after a critical year,
    * continuing with the Gegorian Era.
    * @param criticalYearNumber The year the reform happened.
    * @param criticalYearUnit The shape of that year as a TimeUnit. Typically a certain number of days will be excluded.
    * @return A CalendarSystem with the reformed era plus weeks, set to 1970.
    */

  def switchCalendar(criticalYearNumber: BigInt, criticalYearUnit: TimeUnit) : CalendarSystem = {
    val era = GradedEra startingWith classicJulianEra beginning
      criticalYearNumber have criticalYearUnit beginning
      criticalYearNumber+1 have prolepticGregorianEra

    Calendar(era) setTimestampZero 1970 synchronise planetaryWeekBeginning1970
  }

  /**
    * The Calendar that switches between Julian and Gegorian Era in 1582
    * as determined by Pope Gregor.
    */

  lazy val papalSwitchCalendar = switchCalendar(1582,year1582)

  // Month October in the year 1582, when Pope Gregor ordered switching to his new calendar system.
  lazy val october1582 = month(21) excludingAll (5 to 14)
  lazy val year1582 =   westernYear(julianMonthLengths updated (10,october1582))


  /**
    * The British Empire (including the eastern part of what is now the United States) adopted the Gregorian calendar
    * in 1752, by which time it was necessary to correct by 11 days.
    * Wednesday, 2 September 1752, was followed by Thursday, 14 September 1752.
    */

  lazy val englishSwitchCalendar = switchCalendar(1752,year1752)
  lazy val september1752 = month(19) excludingAll (3 to 13)
  lazy val year1752 =   westernYear(julianMonthLengths updated (9,september1752))

}