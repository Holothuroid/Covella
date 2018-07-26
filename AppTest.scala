package com.github.holothuroid.covella

import com.github.holothuroid.covella.examples._

object AppTest extends App {

  val days = AtomicDays.days

  implicit val cal : Calendar =
    WesternCalendar(days).prolepticGregorianCalendar synchronise MayaCalendar(days).mayaCalendar

  implicit val format : DateFormat = df"$date - ${MayaCalendar.mayaFormat}"

 // val foo = Timestamp(3000).forwards(days).map(_.format).take(500) //.mkString("\n")


 val datum = cal.datum(1957,12,16)

  println( datum.format )

}
