package com.github.holothuroid.covella.examples
import com.github.holothuroid.covella._

case class IndianNationalCalendar(days : TimeUnit = CommonDays.days,
                                  override val prefix: String = "") extends MonthsAndYears {

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

  object leapRule extends PeriodicFunction {
    override def baseNumber: Int = 400

    override def apply(v1: BigInt): Boolean = {
      val y = v1 + 78
      if(y % 400 == 0) true
      else if (y % 100 == 0 ) false
      else if (y % 4 == 0) true
      else false
    }
  }


  val indianNationalEra = Era given leapRule have leapYear default standardYear

  val indianNationalCalendar =
    Calendar(indianNationalEra) setTimestampZero Datum.of('year -> 1891, 'month -> 10, 'day -> 11)
}
