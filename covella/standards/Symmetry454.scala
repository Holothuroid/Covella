package org.covella.standards

import org.covella.TimeUnitInstance
import org.covella.Preamble._
import org.covella.dsl.{Calendar, Era, TimeUnitInstance}

/**
  * A representation of the
  */
object Symmetry454 {
  import CommonDays._
  import PlanetaryWeek._
  import MonthsAndYears._

  val january  = weeks(4) as month named "January" aka ("short","Jan")
  val february = weeks(5) as month named "February" aka ("short","Feb")
  val march = weeks(4) as month named "March" aka ("short","Mar")
  val april  = weeks(4) as month named "April" aka ("short","Apr")
  val may = weeks(5) as month named "May" aka ("short","May")
  val june  = weeks(4) as month named "June" aka ("short","Jun")
  val july = weeks(4) as month named "July" aka ("short","Jul")
  val august = weeks(5) as month named "August" aka ("short","Aug")
  val september = weeks(4) as month named "September" aka ("short","Sep")
  val october  = weeks(4) as month named "October" aka ("short","Oct")
  val november = weeks(5) as month named "November" aka ("short","Nov")
  val december  = weeks(4) as month named "December" aka ("short","Dec")

  val leapDecember = weeks(5) as month named "December" aka ("short","Dec")

  val yearBase : Seq[TimeUnitInstance] = january + february + march + april + may +
                                            june + july + august + september + october + november

  val standardYear = yearBase + december as year
  val leapYear = yearBase + leapDecember as year

  val symmetry454Era = Era given (x =>  x*52 + 146  % 293 < 52 ) have leapYear
  symmetry454Era rest standardYear

  val symmetry454Calendar = Calendar(symmetry454Era)

}
