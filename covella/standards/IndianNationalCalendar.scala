package org.covella.standards

import org.covella.{IrregularUnit, standards}
import org.covella.Preamble._
import org.covella.dsl.{Calendar, Era}

/**
  * A representation of the official national calendar of India.
  */
object IndianNationalCalendar {

  import CommonDays._
  import MonthsAndYears._

  val chaitra = days(30) as month named "Chaitra"
  val vaishakha = days(31) as month named "Vaishākha"
  val jyeshta = days(31) as month named "Jyēshtha"
  val ashadha = days(31) as month named "Āshādha"
  val shravana = days(31) as month named "Shrāvana"
  val bhaadra = days(31) as month named "Bhaadra"
  val ashwin = days(30) as month named "Āshwin"
  val kartika = days(30) as month named "Kārtika"
  val agrahayana = days(30) as month named "Agrahayana"
  val pausha = days(30) as month named "Pausha"
  val magha = days(30) as month named "Māgha"
  val phalunga = days(30) as month named "Phalguna"

  val leapChaitra = days(31) as month named "Chaitra"

  val standardYear = chaitra + vaishakha + jyeshta +
            ashadha + shravana + bhaadra + ashwin + kartika + agrahayana + pausha +
            magha + phalunga as year

  val leapYear = leapChaitra + vaishakha + jyeshta +
    ashadha + shravana + bhaadra + ashwin + kartika + agrahayana + pausha +
    magha + phalunga as year


  val indianNationalEra = Era given ( y =>
    (WesternCalendar.westernEra.getChild(y + 78) ==  WesternCalendar.leapYear) ) have leapYear

  indianNationalEra  rest standardYear

  val indianNationalCalendar = Calendar(indianNationalEra) setTimeStampZero (day>11 & month>10 & year>1891)

}
