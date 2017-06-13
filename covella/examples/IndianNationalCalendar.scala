package com.githup.holothuroid.covella.examples
import com.githup.holothuroid.covella._

/**
  * Created by 1of3 on 12.06.2017.
  */
object IndianNationalCalendar {
 import CommonDays._

  val standardYear = 'year isCycleOf 'month madeFrom days comprising
    ((30,"Chaitra"),
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

  val leapYear = 'year isCycleOf 'month madeFrom days comprising
    ((31,"Chaitra"),
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
    (WesternCalendar.gregorianEra(y + 78) ==  WesternCalendar.leapYear) ) have leapYear

  val indianNationalCalendar = Calendar(indianNationalEra) setTimestampZero "1891-10-11"
}
