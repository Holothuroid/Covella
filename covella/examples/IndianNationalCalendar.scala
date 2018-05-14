package com.github.holothuroid.covella.examples
import com.github.holothuroid.covella._

case class IndianNationalCalendar(days : TimeUnit = CommonDays.days) extends MonthsAndYears {

  val standardYear = year((30,"Chaitra"),
    (31, "Vaishākha"),
    (31, "Jyēshtha"),
    (31, "Āshādha"),
    (31, "Shrāvana"),
    (31, "Bhaadra"),
    (30,"Āshwin"),
    (30, "Kārtika"),
    (30,"Agrahayana"),
    (30, "Pausha"),
    (30,"Māgha"),
    (30, "Phalguna"))

  val leapYear = year((31,"Chaitra"),
      (31, "Vaishākha"),
      (31, "Jyēshtha"),
      (31, "Āshādha"),
      (31, "Shrāvana"),
      (31, "Bhaadra"),
      (30,"Āshwin"),
      (30, "Kārtika"),
      (30,"Agrahayana"),
      (30, "Pausha"),
      (30,"Māgha"),
      (30, "Phalguna"))

  val indianNationalEra = Era given ( y =>
    (WesternCalendar(days).prolepticGregorianEra(y + 78) ==  WesternCalendar(days).leapYear) ) have leapYear default standardYear

  val indianNationalCalendar =
    Calendar(indianNationalEra) setTimestampZero Datum.of('year -> 1891, 'month -> 10, 'day -> 11)
}
