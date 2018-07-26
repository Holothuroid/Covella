package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._

case class BahaiCalendar(days : TimeUnit = CommonDays.days,
                     override val prefix: String = "") extends MonthsAndYears {

  lazy val months  = month(19)
  lazy val intercalaryDays4 = month(4)
  lazy val intercalaryDays5 = month(5)

  lazy val standardYear = year(months*19 :+intercalaryDays4)
  lazy val leapYear = year(months*19 :+intercalaryDays5)

  lazy val era = Era given divisibleBy(4) have leapYear default standardYear

  lazy val bahaiCalendar = Calendar(era)

}
