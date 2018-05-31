package com.github.holothuroid.covella.tests

import com.github.holothuroid.covella._
import com.github.holothuroid.covella.examples.WesternCalendar
import org.scalatest._
import org.scalatest.prop.PropertyChecks


trait CalendarTests {

  this: FlatSpec with Matchers with PropertyChecks =>

  /**
    * This test verifies that the laws for converting between Timestamp and Datum hold for a Calendar.
    */


  def partialInverse(cal: Calendar) =
    it should " turn any Timestamp into a Datum and back " in {

      forAll { (i: Long) => {
        val timestamp = Timestamp(i)

        assert(cal.timestampOrZero(timestamp.inCalendar(cal)).contains(timestamp))
      }
      }

    }

}


class SimpleCalendarsTest extends FlatSpec with Matchers with PropertyChecks with CalendarTests {

  "A calendar of ticks " should behave like partialInverse( Calendar(Tick('foo)) )

  "A calendar with a Measure " should behave like partialInverse( Calendar('bar of (Tick('foo),10) ) )

  "A calendar with Cycle " should behave like partialInverse(
    Calendar(
    'baz.isCycleOf('bar of (Tick('foo),3),'bar of (Tick('foo),5) )
    )
  )



}



class PeriodicEraTest extends FlatSpec with Matchers with PropertyChecks with CalendarTests {

   val periodicCalendar = Calendar(WesternCalendar().prolepticJulianEra)  setTimestampZero Datum.of('year -> 1970)

  "An era constructed with periodic functions " should " result in a periodic era." in {
    assert(periodicCalendar.era.isInstanceOf[PeriodicEra])
  }

  lazy val periodicEra = periodicCalendar.era.asInstanceOf[PeriodicEra]

  "A periodic era " should " have its period start at index 0" in {
    assert(periodicEra(0) contains periodicEra.elements(0))
  }

  it should " have the next period element at index 1" in {
    assert(periodicEra(1) contains periodicEra.elements(1))
  }

  it should "have the last period element  at index -1" in {
    assert(periodicEra(-1) contains periodicEra.elements.last)
  }

  it should "have the second to last period element at index -2" in {
    assert(periodicEra(-2) contains periodicEra.elements.reverse(1))
  }

  it should "take no time to reach index 0" in {
    assert(periodicEra.ticksUntil(0) === BigInt(0))
  }

  it should "take time to reach index 1 equal to the length of the first period element" in {
    assert(periodicEra.ticksUntil(1) === periodicEra.elements(0).ticks)
  }

  it should "take time to reach index -1 equal to the length of the final period element" in {
    assert(periodicEra.ticksUntil(-1).abs === periodicEra.elements.last.ticks )
  }

  it should "take time to reach index -2 equal to the length of two last period elements" in {
    assert(periodicEra.ticksUntil(-2).abs === periodicEra.elements.last.ticks + periodicEra.elements.reverse(1).ticks)
  }

   "A calendar with a periodic era " should behave like partialInverse(periodicCalendar )
}


class ExcludingEraTest extends FlatSpec with Matchers with PropertyChecks with CalendarTests{

  val cal = Calendar( WesternCalendar().prolepticJulianEra excludingShiftPrevious 0 ) setTimestampZero
    Datum.of('year -> 1970)

  "An era with ExcludingShiftPrevious" should behave like partialInverse(cal)
}


class GradedEraTest extends FlatSpec with Matchers with PropertyChecks with CalendarTests{

  val foo = WesternCalendar()

  import foo._

  val era = GradedEra startingWith
    classicJulianEra beginning
    1582 have year1582 beginning
    1583 have prolepticGregorianEra

  val cal = Calendar( era )

  "A calendar with GradedEra" should behave like partialInverse(cal)
}