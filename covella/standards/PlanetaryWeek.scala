package org.covella.standards

import org.covella.Preamble._
import org.covella.dsl.Cycle

/**
  * The typical 7 day week, used in the western world and the middle east.
  */
object PlanetaryWeek {

  import CommonDays.days

  val weeks = Cycle from days("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") named "week"

  def week = weeks
  def w = num(week)(2)
  def W = nam(week)("default")

}
