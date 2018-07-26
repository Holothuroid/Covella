package com.github.holothuroid.covella.examples
import com.github.holothuroid.covella._

/**
  * The solar day of Mars is 39 minutes and 23.x seconds longer than the Terran one.
  * See https://en.wikipedia.org/wiki/Timekeeping_on_Mars
  */

object Sol {
  lazy val millis : TimeUnit = DaysOfMillis.millis
  lazy val sols = 'sol of (millis,88775244)
}

/**
  * The Darian calendar is a proposed system of time-keeping
  * designed to serve the needs of any possible future human settlers on the planet Mars.
  * It was created by aerospace engineer, political scientist,
  * and space jurist Thomas Gangale in 1985 and named by him after his son Darius.
  */

class DarianCalendar {
  import Sol.sols

  lazy val month28 = 'darianMonth of (sols,28) withOffset 1
  lazy val month27 = 'darianMonth of (sols,27) withOffset 1

  lazy val quarter = month28 * 5 :+ month27
  lazy val leapQuarter = month28 * 6

  lazy val monthNames =
    Vector(
      "Sagitarius", "Dhanus",  "Capricornus",   "Makara", "Aquarius",  "Kumbha",
      "Pisces",     "Mina",    "Aries",         "Mesha",  "Taurus",    "Rishabha",
      "Gemini",     "Mithuna",  "Cancer",       "Karka",  "Leo",       "Simha",
      "Virgo",      "Kanya",    "Libra",        "Tula",   "Scorpius",  "Vrishika"
  )

  lazy val standardYear = 'darianYear isCycleOf
    (quarter++quarter++quarter++quarter :_* ) withNames (monthNames :_*) withOffset 1

  lazy val leapYear = 'darianYear isCycleOf
    (quarter++quarter++quarter++leapQuarter :_* ) withNames (monthNames :_*) withOffset 1

  /**
    * The basic intercalation formula therefore allocates six 669-sol years
    * and four 668-sol years to each Martian decade.
    * The former (still called leap years even though they are more common than non-leap years)
    * are years that are either odd (not evenly divisible by 2)
    * or else are evenly divisible by 10, producing 6,686 sols per ten years (668.6 sols per year).
    */

  lazy val darianEra =
    Era given notDivisibleBy(2) have leapYear given divisibleBy(10) have leapYear default standardYear

  lazy val darianCalendar = Calendar(darianEra) setTimestampZero
    Datum.of('darianYear -> 191, 'darianMonth -> 20, 'sol -> 26 )

}
