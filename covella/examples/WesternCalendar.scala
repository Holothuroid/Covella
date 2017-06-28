package com.githup.holothuroid.covella.examples

import com.githup.holothuroid.covella._

/**
  * Created by 1of3 on 10.06.2017.
  */
object CommonDays {

  val millis = Tick('millisecond)
  val seconds = 'second is (millis,1000)
  val minutes = 'minute is (seconds,60)
  val hours = 'hour is (minutes,60)
  val days = 'day is (hours,24)

  val julianDays = 'julianDay isAliasFor days  // Julian Days means counting days from noon on January 1, 4713 BC, proleptic Julian calendar
 val jdCalendar = Calendar(julianDays) setTimestampZero "2440587-12" // Hope this is correct. Calendars are mess. ^^ Calculation courtesy of http://aa.usno.navy.mil/data/docs/JulianDate.php

  val doubleHalfDays = 'day isCycleOf 'halfday madeFrom hours comprising ( (12,"AM"), (12, "PM") )
                // If you want traditional counting of hours, this is a way to do it.

}

object PlanetaryWeek{
import CommonDays._

  val weeks = 'week isCycleOf 'weekday madeFrom days comprising
    ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

  val planetaryWeek = Calendar(weeks)

}


object WesternCalendar {
  import CommonDays._
  import PlanetaryWeek._

  val standardYear = 'year isCycleOf 'month madeFrom days comprising
    ((31, "January"),
    (28, "February"),
    (31, "March"),
    (30, "April"),
    (31, "May"),
    (30, "June"),
    (31, "July"),
    (31, "August"),
    (30, "September"),
    (31, "October"),
    (30, "November"),
    (31, "December"))

  val leapYear =  'year isCycleOf 'month madeFrom days comprising
    ((31, "January"),
    (29, "February"),
    (31, "March"),
    (30, "April"),
    (31, "May"),
    (30, "June"),
    (31, "July"),
    (31, "August"),
    (30, "September"),
    (31, "October"),
    (30, "November"),
    (31, "December"))

   val julianEra = Era given
    (x => divisibleBy(4)(x) && (x != 0)) have leapYear given  // Julian leap rule
    (x => x!=0) have standardYear    // There is no year 0!

  val gregorianEra = Era given
    (x => divisibleBy(1000)(x) && (x != 0)) have leapYear given
    (x => divisibleBy(400)(x) && (x != 0)) have standardYear given // Gregorian leap rule
    (x => divisibleBy(4)(x) && (x != 0)) have leapYear given  // Julian leap rule
    (x => x!=0) have standardYear    // There is no year 0!

  val julianCalendar = Calendar(julianEra).setTimestampZero("1970") synchronise
    planetaryWeek.setTimestampZero("0-4")   // We don't really care about the number of the week, hence 0. The day was a Thursday, hence 4.

  val gregorianCalendar = Calendar(gregorianEra).setTimestampZero("1970")  synchronise
    planetaryWeek.setTimestampZero("0-4")/**/


}