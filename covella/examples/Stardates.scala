package com.github.holothuroid.covella.examples

import com.github.holothuroid.covella._

/**
  * Star Trek TNG stardates working on the following assumptions.
  * Each season beginning from TNG Season 1 spans 1000 stardates as seen on the show.
  * Each season approximately spans one earth year.
  * The first decimal place given is exactly 1 hour. -> Not confirmed. We only know, it's "part of the day".
  * With these assumptions 1000 stardates is 416 days.
  * 3 stardates are 30 hours.
  *
  * For timeStampZero I assume that the 41000s start exactly on January 1st, 2364.
  * This would put Stardate 0 at 112 years before Farpoint, when the TOS era stardate "system" was still in use.
  * But people often put the starting point of their calendars into the past, so this migh still work.
  *
  * An alternate route would be to set 1000 stardates at 365 days, giving you 8 hours for a stardate and 0,8 hours for the decimal place.
  * That puts Stardate 0 at 41 years before Farpoint.
  */

object Stardates {
  import CommonDays._


 val stardates = 'stardate of (hours,10)
 def stardateZero  = ( "2364".dateInCalendar(WesternCalendar().classicGregorianCalendar).begins.get - (41000,stardates) ).value


 val stardateCalendar =  Calendar(stardates,stardateZero)
}


