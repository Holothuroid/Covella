
package org.covella.standards

import org.covella.Cycle
import org.covella.Preamble._
import org.covella.dsl.{Calendar, Cycle, Era}



/**
  * A perpetuous calendar based on famous scientists from A to M. Used in orionsarm.com among others.
  */




object TranquilityCalendar {

  import CommonDays._
  import PlanetaryWeek._
  import MonthsAndYears.{year,years,y,Y}

  val months =  Cycle from  weeks(4) named "month"
  def month = months
  def m = num(months)(2)
  def M = nam(months)("default")

  val firstHalfYear = months("Archimedes", "Brahe",
    "Copernicus", "Darwin", "Einstein", "Faraday", "Galileo", "Hippocrates"  )

  val aldrin = day named "Aldrin Day"
  val secondHalfYear = months( "Imhotep", "Jung", "Kepler", "Lavoisier", "Mendel" )
  val armstrong = day named "Armstrong Day"

  val standardYear = firstHalfYear + secondHalfYear + armstrong as year
  val leapYear = firstHalfYear + aldrin + secondHalfYear + armstrong as year

  val tranquilityEra = Era given
          (y => WesternCalendar.westernEra.getChild(y + 3) ==  WesternCalendar.leapYear)   have leapYear

  tranquilityEra rest standardYear

  tranquilityEra defaultEpochNames(post = "AT", ante = "BT")


  val tranquilityCalendar = Calendar(tranquilityEra) setTimeStampZero ( year>1 & month>6 & day/month>25)

  }


