package com.githup.holothuroid.covella.examples
import com.githup.holothuroid.covella._

/**
  * Created by 1of3 on 12.06.2017.
  */
object Symmetry454 {

  import CommonDays._
  import PlanetaryWeek._

  val standardYear = 'year isCycleOf 'month madeFrom weeks comprising
    ((4, "January"),
      (5, "February"),
      (4, "March"),
      (4, "April"),
      (5, "May"),
      (4, "June"),
      (4, "July"),
      (5, "August"),
      (4, "September"),
      (4, "October"),
      (5, "November"),
      (4, "December"))

  val leapYear =  'year isCycleOf 'month madeFrom days comprising
    ((4, "January"),
      (5, "February"),
      (4, "March"),
      (4, "April"),
      (5, "May"),
      (4, "June"),
      (4, "July"),
      (5, "August"),
      (4, "September"),
      (4, "October"),
      (5, "November"),
      (5, "December"))  // <= leap weak!

  val sym454Era = Era given (x => ( 52*x+146)%293<52) have leapYear default standardYear
  val sym454Calendar = Calendar(sym454Era) setTimestampZero "1970"

}
