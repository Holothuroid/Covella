package com.github.holothuroid.covella.tests

import com.github.holothuroid.covella._
import com.github.holothuroid.covella.examples.WesternCalendar
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

trait JavaConformityTests  {

  this:  FlatSpec with Matchers with PropertyChecks =>

  import java.time.Instant

  def javaTimeInstantOfEpochSeconds(cal: Calendar,
                                    starting: Long = Instant.MIN.getEpochSecond,
                                    until : Long = Instant.MAX.getEpochSecond) =
    it should " should interpret Timestamps in the same way as java.time.Instant " in {


    val dfpre = df"$y-$m-${d}T$h:$min:${s}Z"
    val pattern1 = "-(\\d-.*)".r
    val pattern2 = "-(\\d{2,2}-.*)".r
    val pattern3 = "-(\\d{3,3}-.*)".r

    val correctedFormat : Datum=> String =  dfpre.format.andThen(
      _ match {
        case pattern1(group) => "-000" + group
        case pattern2(group) => "-00" + group
        case pattern2(group) => "-0" + group
        case s : String => s
      }
    )

    val df = dfpre.copy(format = correctedFormat)

    forAll { (i: Long) => { whenever( i>=starting && i<until ) {
        Instant.ofEpochSecond(i).toString.filterNot(_  == '+') shouldBe
          Timestamp(i).inCalendar(cal).format(df).filterNot(_  == '+')
      }
    }

    }

  }
}


class GregorianEraTest extends  FlatSpec
  with Matchers with PropertyChecks with JavaConformityTests {

    val cal = WesternCalendar().prolepticGregorianCalendar

    "The Gregorian Calendar" should behave like javaTimeInstantOfEpochSeconds(cal)

}
