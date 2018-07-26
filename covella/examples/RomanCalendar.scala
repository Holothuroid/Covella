package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._

/**
  * This is a representation of the Roman Republican Calendar as it might have worked,
  * based on Macrobius.
  * This attempt DOES NOT represent what actually happened historically,
  * as the Romans failed or fudged the rather complex rules from time to time.
  * As such, this model does not include sensible starting points or the consul lists.
  *
  * This model starts the year with January and thus uses the civil cycle at the end of the republic.
  * The religious year (and in earlier times the civil year) starts in March,
  * after the shorter Cleansing Month ('February').
  *
  * Leap months are inserted in 24 year cycle, aborting February after the Terminalia festival.
  * @param days A TimeUnit. By default `CommonDays.days`.
  * @param prefix Optional prefix to prevent shadowing in CalendarSystems.
  */

case class RomanCalendar (days : TimeUnit = CommonDays.days,
                          override val prefix: String = "") extends MonthsAndYears {


  lazy val romanCalendar = Calendar(romanEra) synchronise nundinalCycle

  lazy val romanEra = PeriodicEra(fiveTetraeteris ++ sixthTetraeteris)

  lazy val tetraeteris = Vector(commonYear,shortLeapYear,commonYear,longLeapYear)
  lazy val sixthTetraeteris = Vector(commonYear,reducedLeapYear,commonYear,commonYear)

  private def fiveTetraeteris  = (1 to 5).flatMap(_ => tetraeteris)

  lazy val commonYear = year (
    (29,"January"),
    (28,"February"),
    (31,"March"),
    (29,"April"),
    (31,"May"),
    (29,"June"),
    (31,"Quintilis"),
    (29,"Sexitilis"),
    (29,"September"),
    (31,"October"),
    (29,"November"),
    (29,"December")
  )

  lazy val shortLeapYear = year (
    (29,"January"),
    (23,"February"),  // February aborted after day 23, Terminalia festival
    (27,"Intercalaris"), // +22 days netto
    (31,"March"),
    (29,"April"),
    (31,"May"),
    (29,"June"),
    (31,"Quintilis"),
    (29,"Sexitilis"),
    (29,"September"),
    (31,"October"),
    (29,"November"),
    (29,"December")
  )

  lazy val longLeapYear = year (
    (29,"January"),
    (23,"February"),  // February aborted after day 23, Terminalia festival
    (28,"Intercalaris"), // +23 days netto
    (31,"March"),
    (29,"April"),
    (31,"May"),
    (29,"June"),
    (31,"Quintilis"),
    (29,"Sexitilis"),
    (29,"September"),
    (31,"October"),
    (29,"November"),
    (29,"December")
  )

  lazy val reducedLeapYear = year (
    (29,"January"),
    (23,"February"),  // February aborted after day 23, Terminalia festival
    (26,"Intercalaris"), // +21 days netto
    (31,"March"),
    (29,"April"),
    (31,"May"),
    (29,"June"),
    (31,"Quintilis"),
    (29,"Sexitilis"),
    (29,"September"),
    (31,"October"),
    (29,"November"),
    (29,"December")
  )

  private lazy val nundinalDays = 'nundinalDay isAliasFor days
  lazy val nundinalCycle = Calendar( 'nundinalCycle of nundinalDays withNames
    ("A","B","C","D","E","F","G","H") withOffset 1 )


  // todo: Provide the well beloved date format.

  // todo: Add day qualities

}
