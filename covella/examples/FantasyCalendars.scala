package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._

/**
  * A calendar used in the D&D setting Faerun a.k.a. The Forgotten Realms.
  */

case class HarptosCalendar(days : TimeUnit = CommonDays.days){
  lazy val tendays = 'tenday of (days,3) withOffset 1
  lazy val month = 'month of (tendays,3) withOffset 1

  def holiday(name: String) = epagomenal(days,name,'month,'tenday)

  lazy val standardYear = 'year cycles (
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

 lazy val leapYear = 'year cycles (
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

  lazy val altMonths = Map(
    "Hammer" -> (1,"Deepwinter"),
    "Alturiak" -> (2,"The Claw of Winter"),
    "Ches" -> (3,"The Claw of Sunsets"),
    "Tarsakh" -> (4,"The Claw of Storms"),
    "Mirtul" -> (5,"The Melting"),
    "Kythorn" -> (6,"The Time of Flowers"),
    "Flamerule" -> (7,"Summertide"),
    "Eleasis" -> (8,"Highsun"),
    "Eleint" -> (9,"The Fading"),
    "Marpenoth" -> (10,"Leafall"),
    "Uktar" -> (11,"The Rotting"),
    "Nightval" -> (12,"The Drawing Down")
  )

  def getAltMonth(datum: Datum) : (Int,String) =
    datum.getName('month).map(altMonths).getOrElse(null)

  lazy val harptosEra = Era given divisibleBy(4) have leapYear default standardYear

  lazy val harptosCalendar = Calendar(harptosEra) add ('altMonth-> getAltMonth)
}


/**
  * Calendar for the Eberron campaign setting for D&D by Keith Baker.
  */

case class EberronCalendar(days : TimeUnit = CommonDays.days){

  lazy val weeks = 'week of days withNames("Sul","Mol","Zol","Wir","Zor","Far","Sar")

  lazy val months = 'month of (weeks, 4)

  lazy val years = 'year of months withNames(
    "Zarantyr","Olarune","Therendor",
    "Eyre","Dravago","Nymm",
    "Lharvion","Barrakas","Rhaan",
    "Sypheros","Aryth","Vult"
  )


  lazy val solarCalendar = Calendar(years) setTimestampZero 998

  // Name, Orbit, RemainsNotable, StartingPosition

  private val planeData = Vector(
    ("Danvi",134400,33600,50400),
    ("Dolurrh",33600,336,8559),
    ("Fernia",1680,28,1344),
    ("Irian",1008,10,420),
    ("Lamannia",364,7,94),
    ("Mabar",1680,3,422),
    ("Risia",1680,28,505),
    ("Shavarath",12096,336,4032),
    ("Syrania",3360,1,841),
    ("Thelanis",75600,2352,20076),
    ("Xoriat",2352000,336,588168)
  )

  def planeNames = planeData.map(_._1)

  lazy val planesFactory = Satellites.withPhases(("Remote","Waxing"),("Coterminous","Waning"))

  /**
    * Collection of planes orbiting the world of Eberron.
    * Except for Dal Quor, which is always remote, and Kythri which is utterly random.
    */

  lazy val planes : Vector[Calendar] = planeData.map{
    case (name,orbit,remainsNotable,startingPosition) =>
      {
      val phase = if (startingPosition < orbit/2) 1 else 2
      val phaseTicks = (startingPosition % (orbit/2)) * days.ticks

      planesFactory.make(name.toLowerCase,orbit,days,remainsNotable) setTimestampZero (0,phase,phaseTicks)
      }
    }


  lazy val eberronCalendar : Calendar = planes.foldLeft (CalendarSystem(Vector(solarCalendar )))
    { case (sys: Calendar,simple: Calendar) => sys synchronise simple}

}