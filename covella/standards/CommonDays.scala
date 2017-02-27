package org.covella.standards

import org.covella.Tick
import org.covella.Preamble._
import org.covella.dsl.{Cycle, Tick}

/**
  * These are days as most people understand them. Without leap seconds.
  */
object CommonDays {

  var millis = Tick("millisecond") countFromZero
  var seconds = Cycle from millis(1000) named "second" countFromZero
  var minutes = Cycle from seconds(60) named "minute" countFromZero
  var hours = Cycle from minutes(60) named "hour" countFromZero
  var days= Cycle from hours(24) named "day"


  def milli = millis
  def second = seconds
  def minute = minutes
  def hour = hours
  def day = days


  def mil = num(millis)(2)
  def Mil = nam(millis)(_)
  def d = num(day)(2)
  def D = nam(day)(_)
  def h = num(hour)(2)
  def H = nam(hour)(_)
  def min = num(minute)(2)
  def Min = nam(minute)(_)
  def s = num(second)(2)
  def S = nam(second)(_)



}
