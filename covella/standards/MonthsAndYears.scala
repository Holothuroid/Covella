package org.covella.standards

import org.covella.Preamble.{nam, num}
import org.covella.dsl.IrregularUnit

/**
  * Empty defintions for months and years with convenience functions.
  */
object MonthsAndYears {

  val months= IrregularUnit("month")
  val years = IrregularUnit("year")

  def month = months
  def year = years

  def y = num(year)(2)
  def Y = nam(year)("default")
  def m = num(month)(2)
  def M = nam(month)("default")

}
