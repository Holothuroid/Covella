package com.github.holothuroid.covella.tests

import com.github.holothuroid.covella._
import com.github.holothuroid.covella.examples.WesternCalendar
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

trait JavaConformityTests  {

  this:  FlatSpec with Matchers with PropertyChecks =>

  def javaTimeInstantOfEpochSeconds(cal: Calendar) =
    it should " should interpret Timestamps in the same way as java.time.Instant " in {

    val df = df"$y-$m-${d}T$h:$min:${s}Z" // this DateFormat doesn't fit for negative years with less than 4 digits

      forAll { (i: Int) => {

        java.time.Instant.ofEpochSecond(i).toString.replaceFirst("\\[0","0") shouldBe
          Timestamp(i).inCalendar(cal).format(df).replaceFirst("\\[0","0")
      }

    }

  }
}


class GregorianEraTest extends  FlatSpec
  with Matchers with PropertyChecks with JavaConformityTests {

    val cal = WesternCalendar().classicGregorianCalendar

    "The Gregorian Calendar" should behave like javaTimeInstantOfEpochSeconds(cal)

}
