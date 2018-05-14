package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._

/**
  * A calendar used in the D&D setting Faerun a.k.a. The Forgotten Realms.
  */

case class HarptosCalendar(days : TimeUnit = CommonDays.days){
val tendays = 'tenday of (days,3) withOffset 1
val month = 'month of (tendays,3) withOffset 1

def holiday(name: String) = epagomenal(days,name,'month,'tenday)

val standardYear = 'year cycles (
  (month,"Hammer"),
  holiday("Midwinter"),
  (month,"Altariuk"),
  (month,"Ches"),
  (month,"Tarsakh"),
  holiday("Greengras"),
  (month,"Mirtul"),
  (month,"Kythorn"),
  (month,"Flamrule"),
  holiday("Midsummer"),

  (month,"Eleasis"),
  (month,"Eleint"),
  holiday("Highharvestide"),
  (month,"Marpenoth"),
  holiday("Feast of the Moon"),
  (month,"Nightval")
  )

  val leapYear = 'year cycles (
    (month,"Hammer"),
    holiday("Midwinter"),
    (month,"Altariuk"),
    (month,"Ches"),
    (month,"Tarsakh"),
    holiday("Greengras"),
    (month,"Mirtul"),
    (month,"Kythorn"),
    (month,"Flamrule"),
    holiday("Midsummer"),
    holiday("Shieldmeet"),  // leap day
    (month,"Eleasis"),
    (month,"Eleint"),
    holiday("Highharvestide"),
    (month,"Marpenoth"),
    holiday("Feast of the Moon"),
    (month,"Nightval")
)

val harptosEra = Era given divisibleBy(4) have leapYear default standardYear

val harptosCalendar = Calendar(harptosEra)
}