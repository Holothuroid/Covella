package com.github.holothuroid.covella

import com.github.holothuroid.covella.examples.EberronCalendar


object AppTest extends App{

  val cal3 = EberronCalendar(AtomicDays.days).eberronCalendar

  val luna = Satellites.withPhases(("foo","bar"),("baz","bak")).make("luna",29.5) setTimestampZero (0,2,1)

  println(Timestamp(2007000).inCalendar(cal3))



}


