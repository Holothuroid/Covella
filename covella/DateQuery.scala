package com.github.holothuroid.covella

/**
  * Created by 1of3 on 04.06.2017.
  */

/**
  * DateQuery is a small DSL by algebraic data types, to describe intervals.
  */


trait DateQuery {
  def execute(implicit cal: Calendar) : Option[Interval]

}

trait DateConstrain extends DateQuery

object DateQuery {
  implicit def datum2Lifted(d: Datum) : Lifted = Lifted(d)
  implicit def timestamp2Fixpoint(t: Timestamp) : Fixpoint = Fixpoint(t)
}


case class Lifted(datum: Datum) extends DateQuery {
  def execute(implicit cal: Calendar) : Option[Interval] = ???

}

case class Fixpoint(value: Timestamp) extends DateQuery {
  def execute(implicit cal: Calendar) : Option[Interval] = Some(Interval(value,value))

}

/*
trait CombinedQuery extends DateQuery {

  val a: DateQuery
  val b: DateConstrain

  def executeImpl(x: Interval, y: Interval): Option[Interval]

  def execute(implicit cal: Calendar) = b.execute match {
    case Some(x) =>
      a.execute match {
        case Some(y) => executeImpl(x, y)
        case None => None
      }
    case None => None
  }

}

case class While(a : DateQuery, b: DateQuery) extends CombinedQuery {
  def executeImpl(a: Interval, b: Interval) : Option[Interval] = a intersect b

  }


case class Before(a : DateQuery, b: DateQuery)extends CombinedQuery  {
  def executeImpl(a: Interval, b: Interval) : Option[Interval] = ???

}

case class After(a : DateQuery, b: DateQuery) extends CombinedQuery {
  def executeImpl(a: Interval, b: Interval) : Option[Interval] = ???

}

case class AfterBegins(a : DateQuery, b: DateQuery) extends CombinedQuery {
  def executeImpl(a: Interval, b: Interval) : Option[Interval] = ???


}


case class BeforeEnds(a : DateQuery, b: DateQuery) extends CombinedQuery {
  def executeImpl(a: Interval, b: Interval) : Option[Interval] = ???

}

*/

