/**
  * Created by 1of3 on 17.10.2016.
  *
  * DateFormat hat eine Funktion to, um ein Datum zum String zu machen
  * Hat eine partielle Funktion from, um einen String zu parsen.
  * Implementierung nicht vollständig. Todo: Methoden, um Daten anhand eines Kalenders zu finden.
  * DateFormatFactory benutzt StringContext, um Datenformat zu erzeugen, z.B. date"""$number(day). $name(month) number(year)"""
  * Todo: DSL benötigt Methode "format", um ein Format hinzuzufügen.
  */

package org.covella.dsl

import org.covella.dates.{DateLike, Datum}

case class DateFormat(makeString: DateLike => String, fromString: PartialFunction[String,Datum])

case class DateFormatHelper(to: DateLike => Any, from: String => (TimeUnit,Int))



