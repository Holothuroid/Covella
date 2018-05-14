package com.github.holothuroid.covella


/**
  * DateFormats include methods to turn Strings into Datums and back.
  *
  * @param parse Turns a String into a Datum. The Datum is unchecked and not necessarily valid in any Calendar.
  * @param format Formats a Datum. This should work irrespective of the presence of a Calendar.
  * @param placeholder Used when creating composite DateFormates via df"..." (DateFormatFactory)
  */

case class DateFormat(parse: String=>Datum, format: Datum=>String, placeholder : String = ".*")
