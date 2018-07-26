package com.github.holothuroid.covella.examples
import com.github.holothuroid.covella._

/**
  * Reform proposal Symmetry454
  * 4 quarters in a year of 3 months each
  * First month in quarter 4 weeks, then 5 weeks, then 4 weeks
  * Leap week in December
  * https://en.wikipedia.org/wiki/Symmetry454
  */

case class Symmetry454(days : TimeUnit = CommonDays.days) {

  val weeks = PlanetaryWeek(days).weeks

  val weeks4 = 'month of (weeks,4) withOffset 1
  val weeks5 = 'month of (weeks,5) withOffset 1

  lazy val monthNames = Vector("January", "February", "March", "April", "May",
    "June","July","August","September","October","November","December")

  val standardYear = 'year isCycleOf (
    weeks4,weeks5,weeks4,
    weeks4,weeks5,weeks4,
    weeks4,weeks5,weeks4,
    weeks4,weeks5,weeks4,
  ) withNames(monthNames : _*) withOffset 1

  val leapYear =  'year isCycleOf (
    weeks4,weeks5,weeks4,
    weeks4,weeks5,weeks4,
    weeks4,weeks5,weeks4,
    weeks4,weeks5,weeks5,  // Leap week in December
  ) withNames(monthNames : _*) withOffset 1


  /**
    * The standard periodic functions in package covella do not capture this calendar's rather complex leap rule.
    * Good thing, we can implement one for this very purpose.
    */

  case object leapRule extends Function1[BigInt,Boolean] with PeriodicFunction{
    override def baseNumber: Int = 293
    override def apply(i: BigInt): Boolean = ((52*i.abs+146)%293)<52
  }

  val sym454Era = Era given leapRule have leapYear default standardYear
  val sym454Calendar = Calendar(sym454Era)

}


/** A perpetuous calendar based on famous scientists from A to M. Used in orionsarm.com among others.
  * The date 0001-01-01 is the moon landing 1969-07-21 Gregorian.
  * https://en.wikipedia.org/wiki/Tranquility_Calendar
  * */


case class TranquilityCalendar(days : TimeUnit = CommonDays.days) extends {

  lazy val weeks = 'week of days withNames
    ("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") withOffset 1 // Weekdays are counted from 1, not 0.

  lazy val months = 'month of (weeks,4) withOffset 1

  def armstrong =
    epagomenal(CommonDays.days,"Armstrong Day",'month,'week)

  def aldrin  =
    epagomenal(CommonDays.days,"Aldrin Day",'month,'week)

  def first8Months = Vector("Archimedes", "Brahe",
  "Copernicus", "Darwin", "Einstein", "Faraday", "Galileo", "Hippocrates"  )
    .map((months,_))

  def second5Months = Vector( "Imhotep", "Jung", "Kepler", "Lavoisier", "Mendel" )
    .map((months,_))

  val standardYear = 'year cycles (first8Months ++ second5Months :+ armstrong : _*) withOffset 1
  val leapYear = 'year cycles ((first8Months :+ aldrin) ++ second5Months :+ armstrong : _*) withOffset 1

  val tranquilityEra = Era given congruent(3,4) have leapYear default standardYear
  // todo: Should have same leap years as GregorianCalendar. Now kinda proplepticJulian

  def moonlandingTill1970  =
    ( "1969-07-21".dateInCalendar(WesternCalendar().classicGregorianCalendar).begins.get + (164,days) ).value

  val tranquilityCalendar = Calendar(tranquilityEra,moonlandingTill1970)


}