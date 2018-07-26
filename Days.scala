package com.github.holothuroid.covella

/**
  * The examples in this package are parametrized with days, to allow for different orders of precision.
  */

trait Days {
  def days: TimeUnit
}


/**
  * The most simple possibility: A day is a tick on the clock.
  * Use this, if you are never interested in times of the day.
  */

object AtomicDays extends Days{
  lazy val days = Tick('day)
}

/**
  * Days counting seconds. The most common way of measuring time.
  * Days have all the same length; no leap seconds.
  */

object CommonDays extends Days {
  lazy val seconds = Tick('second)
  lazy val minutes = 'minute of (seconds,60)
  lazy val hours = 'hour of (minutes,60)
  lazy val days = 'day of (hours,24)

  lazy val doubleHalfDays = 'day cycles hours as 'halfDay comprising ( (12,"AM"), (12, "PM") ) withOffset 1
  // If you want traditional counting of hours, this is a way to do it.
}

/**
  * Similar to CommonDays, but with milliseconds as ticks on the clock.
  * Use this for interoperability with Java.
  */

object DaysOfMillis extends Days {
  lazy val millis = Tick('millisecond)
  lazy val seconds = 'second of (millis,1000)
  lazy val minutes = 'minute of (seconds,60)
  lazy val hours = 'hour of (minutes,60)
  lazy val days = 'day of (hours,24)

  lazy val doubleHalfDays = 'day cycles hours as 'halfDay comprising ( (12,"AM"), (12, "PM") ) withOffset 1
}


/**
  * Continous counting of days starting at noon January 1, 4713 BC, proleptic Julian calendar.
  * Mostly used by historians and astronomers. Developed by Joseph Scaliger in 1583.
  *
  */

case class JulianDays(days : TimeUnit = CommonDays.days){
   // Hope this is correct. Calculation courtesy of http://aa.usno.navy.mil/data/docs/JulianDate.php
  lazy val jdCalendar = Calendar(days) setTimestampZero Datum.of('day -> 2440587, 'hour->12)
}