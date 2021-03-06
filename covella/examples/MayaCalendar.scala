package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._

/**
  * The calendar of the Maya people, as best as I understand it.
  * Includes Tzolkin, Haab, Long Count and Lords of Night.
  * Timestamps are set in regard to 1970-01-01 CE for synchronisation with other calendars.
  * @param days The precision of timekeeping. By standard, counting seconds.
  */

case class MayaCalendar(days: TimeUnit = CommonDays.days) {

  /** Long count
  * Twenty of these k'ins [days] are known as a winal or uinal.
  * Eighteen winals make one tun.
  * Twenty tuns are known as a k'atun.
  * Twenty k'atuns make a b'ak'tun. */

  lazy val longCountCalendar = Calendar(baktuns) setTimestampZero
    Datum.of(
      'baktun -> 12,
      'katun -> 17,
      'tun -> 16,
      'winal -> 7,
      'kin ->5) // This corresponds to 1970-01-01, i.e. unix epoch

  lazy val kins = 'kin isAliasFor days
  lazy val winals = 'winal of (kins,20)
  lazy val tuns = 'tun of (winals,18)
  lazy val katuns = 'katun of (tuns,20)
  lazy val baktuns = 'baktun of (katuns,20)

   /** Haab
    18 months of 20 days and 5 empty days */

  lazy val haabCalendar = Calendar(haabYears) setTimestampZero
    Datum.of('haabYear -> 0, 'haabMonth-> 14, 'haabMonthDay -> 3) // 3 K'ank'in

  // Alias is required to prevent variable shadowing
  private lazy val haabMonthDays = 'haabMonthDay isAliasFor days
  lazy val haabMonth = 'haabMonth of (haabMonthDays,20)
  lazy val wayebs = 'haabMonth of (haabMonthDays,5)

  lazy val haabYears = 'haabYear isCycleOf  ( haabMonth*18 :+ wayebs :_*) withNames
    ("Pop","Wo'","Sip","Sotz'","Sek",
      "Xul","Yaxk'in'","Mol","Ch'en","Yax",
      "Sak'","Keh","Mak","K'ank'in","Muwan'",
      "Pax","K'ayab", "Kumk'u","Wayeb'") withOffset 1 // months are counted from 1


  /** Tzolkin
  * Year consisting of two cycles:
  * - A cycle of numbers ranging from 1 to 13
  * - A cycle of 20 names.
  * Thus forming a year of 260 days.
  * Modeled here by synchronizing two SimpleCalendars. */

  lazy val tzolkinCalendar =
    Calendar(tzolkinNumbers) setTimestampZero
      Datum.of('tzolkinNumberCycle-> 0, 'tzolkinNumberDay->13) synchronise
      Calendar(tzolkinNames).setTimestampZero(
        Datum.of('tzolkinNameCycle->0, 'tzolkinNameDay ->5 ))  // = Chickchan


  // Aliases are required to prevent variable shadowing
  private lazy val tzolkinNumberDays = 'tzolkinNumberDay isAliasFor days
  private lazy val tzolkinNameDays = 'tzolkinNameDay isAliasFor days

  lazy val tzolkinNumbers =
    'tzolkinNumberCycle of (tzolkinNumberDays,13) withOffset 1

  lazy val tzolkinNames = 'tzolkinNameCycle of tzolkinNameDays withNames
    ("Imix'", "Ik'", "Ak'b'al", "K'an", "Chickchan",
      "Kimi", "Manik'", "Lamat", "Muluk", "Ok",
      "Chuwen", "Eb'", "B'en", "Ix", "Men",
      "K'ib'", "Kab'an", "Etz'nab'", "Kwak", "Ajaw"
    ) withOffset 1

  /**
    * The Haab and Tzolkin Calendar round of approximately 52 years.
    */

  lazy val calendarRound = calendarRoundPre add ('yearBearer -> calculateYearBearer  )

  private[covella] lazy val calendarRoundPre = haabCalendar synchronise tzolkinCalendar

  private def calculateYearBearer(datum: Datum) : (Int,String)  =
    datum.get('haabYear)
    .map(x => Datum.of('haabYear -> x))
      .map(_.completeWithCalendar(calendarRoundPre))
      .flatMap(_.begins)
      .map(_.inCalendar(calendarRoundPre))
      .map(x => (x.get('tzolkinNumberDay).getOrElse(BigInt(-1)).toInt,
        x.getName('tzolkinNameDay).getOrElse("UNKNOWN_TZOLKIN")))
      .getOrElse((0,"UNKNOWN_YEARBEARER"))


  /**
    * Lords of Night are a cycle of 9 days, each dedicated to a supernatural entity.
    * Since the Maya names are unknown they are usually given as G1 - G9.
    * 1970-01-01 CE happens to be G1.
    */

  lazy val lordsOfNightCalendar = Calendar(lordsOfNight)

  private lazy val nights = 'lordOfNight isAliasFor days
  lazy val lordsOfNight = 'lordOfNightCycle of (nights,9) withOffset 1

  /**
    * The Long Count combined with the Haab and Tzolkin Calendar Round as well as the Lords of Night.
    */

  lazy val mayaCalendar = longCountCalendar synchronise calendarRound synchronise lordsOfNightCalendar

}


object MayaCalendar{

  /**
    * Includes DateFormats for the single places of the Long Count.
    */

  object LongCountParts {
    lazy val k = num('kin)
    lazy val w = num('winal)
    lazy val t = num('tun)
    lazy val kat = num('katun)
    lazy val b = num('baktun)
  }

  import LongCountParts._


  /**
    * Long Count date format giving numbers for baktun, katun, tun, winal and kin.
    */
  lazy val longCountFormat : DateFormat = df"$b.$kat.$t.$w.$k"


  /**
    *  DateFormat for Haab, giving number of day in month and month name.
    */

  lazy val haabFormat = df"${num('haabMonthDay)} ${nam('haabMonth)}"


  /** DateFormat for Tzolkin, giving number and name.
    */
  lazy val tzolkinFormat = df"${num('tzolkinNumberDay)} ${nam('tzolkinNameDay)}"


  /**
    * DateFormat combining Tzolkin and Haab formats.
    */
  lazy val calendarRoundFormat : DateFormat = df"$tzolkinFormat $haabFormat"

  /**
    * DateFormat giving the Lord of Night for a date as G1 - G9.
    */
  lazy val lordsOfNightFormat = df"G${num('lordOfNight,1)}"

  /**
    * DateFormat extracting the Year Bearer from with number and name from a Datum.
    */

  lazy val yearBearerFormat = df"${num('yearBearer)} ${nam('yearBearer)}"


  /**
    * DateFormat with Long Count, Tzolkin, Haab, Year Bearer and Lord of Night
    */

  lazy val mayaFormat = df"$longCountFormat $tzolkinFormat $haabFormat, $lordsOfNightFormat, YB: $yearBearerFormat"


}