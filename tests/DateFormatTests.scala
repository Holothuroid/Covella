package com.github.holothuroid.covella.tests

import org.scalatest.{Matchers, _}

class DateFormatTests extends FlatSpec with Matchers {

  import com.github.holothuroid.covella._

  "DateFormatFactory" should "compose `num`" in {
    assert( time.parse("01:01:01") === (Datum.of('hour->1, 'minute->1, 'second->1)) )
  }

  it should "create invertible formats" in {
    assert ( time.format( time.parse("01:01:01") ) === "01:01:01" )
  }

  it should "compose composite formats" in {
    val someDate = "0001-01-01"
    val someTime = "01:01:01"
    val someDateTime= "0001-01-01 01:01:01"

    assert ( datetime.parse(someDateTime) === (date.parse(someDate) & time.parse(someTime) ))
  }

}
