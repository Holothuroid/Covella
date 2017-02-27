package org.covella.standards

import org.covella.dsl._
import org.covella.Preamble._




/**
  * The common madness most of us use every day.
  */
object WesternCalendar {


  import CommonDays._
  import PlanetaryWeek._
  import MonthsAndYears._


  val january  = days(31) as month named "January" aka ("short","Jan")
  val february = days(28) as month named "February" aka ("short","Feb")
  val march = days(31) as month named "March" aka ("short","Mar")
  val april  = days(30) as month named "April" aka ("short","Apr")
  val may = days(31) as month named "May" aka ("short","May")
  val june  = days(30) as month named "June" aka ("short","Jun")
  val july = days(31) as month named "July" aka ("short","Jul")
  val august = days(31) as month named "August" aka ("short","Aug")
  val september  = days(30) as month named "September" aka ("short","Sep")
  val october  = days(31) as month named "October" aka ("short","Oct")
  val november = days(30) as month named "November" aka ("short","Nov")
  val december  = days(31) as month named "December" aka ("short","Dec")


  val standardYear = january + february + march + april + may + june + july +
    august + september + october + november + december as year

  val leapFebruary = day(29) as month named "February"aka ("short","Feb")
  val leapYear = january + leapFebruary + march + april + may + june + july +
    august + september + october + november + december as year

  val october1582 = october exclude (5 to 14)
  val year1582 = january + february + march + april + may + june + july +
    august + september + october1582 + november + december as year

  val westernEra =  Era given 1582 have year1582
  westernEra given (x => x > 1582 && (x divisibleBy 100) && (x notDivisibleBy 400)) have standardYear
  westernEra given (_ divisibleBy 4) have leapYear
  westernEra rest standardYear
  westernEra exclude 0

  westernEra defaultEpochNames(post = "CE", ante = "BCE")

  val westernCalendar = Calendar(westernEra) setTimeStampZero(year > 1970)
  westernCalendar synchronize(week,4)

  westernCalendar addFormat ("international" -> df"$y-$m-$d")
  westernCalendar addFormat ("american" -> df"$m/$d/$y")
  westernCalendar addFormat ("german" -> df"$d.$m.$y")
}
